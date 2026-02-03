package dev.bosatsu

import Value._
import cats.data.NonEmptyList

/** UI module for reactive UI with direct DOM updates.
  *
  * Uses static analysis on TypedExpr to extract state->DOM bindings at compile time.
  * At runtime, state changes trigger O(1) direct DOM property updates.
  *
  * VNode represents a virtual DOM node that can be:
  * - Element: h(tag, props, children)
  * - Text: text(content)
  * - Fragment: fragment(children)
  *
  * State[A] is a reactive state container. Reading and writing state
  * is tracked so that bindings can be established at compile time.
  */
object UI {

  /** Loads a file *at compile time* as a means of embedding external files into
    * strings. This lets us avoid resources which complicate matters for
    * scalajs.
    */
  private[bosatsu] inline def loadFileInCompile(file: String): String =
    ${ Macro.loadFileInCompileImpl('file) }

  /** String representation of the UI module */
  val uiString: String =
    loadFileInCompile("core/src/main/resources/bosatsu/ui.bosatsu")

  val packageName: PackageName =
    PackageName.parse("Bosatsu/UI").get

  /** VNode ADT - represents virtual DOM nodes for UI construction */
  sealed trait VNode derives CanEqual
  case class VElement(tag: String, props: List[(String, String)], children: List[VNode]) extends VNode
  case class VText(content: String) extends VNode
  case class VFragment(children: List[VNode]) extends VNode

  /** State container for reactive values */
  case class UIState[A](id: String, var value: A)

  /** List state container for dynamic lists */
  case class UIListState[A](id: String, var items: List[A])

  /** Event handler wrapper */
  case class EventHandler(eventType: String, handler: Value)

  /** Convert Bosatsu list of tuples to Scala list of string pairs */
  private def propsToList(v: Value): List[(String, String)] = {
    VList.unapply(v) match {
      case Some(items) =>
        items.flatMap { item =>
          item match {
            case ProductValue(k, v) =>
              (k, v) match {
                case (Str(kStr), Str(vStr)) => Some((kStr, vStr))
                case _ => None
              }
            case _ => None
          }
        }
      case None => Nil
    }
  }

  /** Convert Bosatsu list of VNodes to Scala list */
  private def childrenToList(v: Value): List[VNode] = {
    VList.unapply(v) match {
      case Some(items) =>
        items.flatMap { item =>
          item.asExternal.toAny match {
            case vn: VNode => Some(vn)
            case _ => None
          }
        }
      case None => Nil
    }
  }

  /** Counter for generating unique state IDs */
  private var stateCounter: Int = 0

  /** JVM externals for UI operations */
  val jvmExternals: Externals =
    Externals.empty
      // state(initial) -> State[a]
      .add(packageName, "state", FfiCall.Fn1 { initial =>
        stateCounter += 1
        ExternalValue(UIState(s"state_$stateCounter", initial))
      })
      // h(tag, props, children) -> VNode
      .add(packageName, "h", FfiCall.Fn3 { (tag, props, children) =>
        val tagStr = tag match { case Str(s) => s; case _ => "div" }
        val propsList = propsToList(props)
        val childList = childrenToList(children)
        ExternalValue(VElement(tagStr, propsList, childList))
      })
      // text(content) -> VNode
      .add(packageName, "text", FfiCall.Fn1 { content =>
        val str = content match { case Str(s) => s; case _ => content.toString }
        ExternalValue(VText(str))
      })
      // fragment(children) -> VNode
      .add(packageName, "fragment", FfiCall.Fn1 { children =>
        val childList = childrenToList(children)
        ExternalValue(VFragment(childList))
      })
      // read(state) -> a
      // Note: In JVM, State is opaque - actual reading happens at runtime
      .add(packageName, "read", FfiCall.Fn1 { state =>
        state.asExternal.toAny match {
          case UIState(_, value: Value @unchecked) => value
          case _ => state
        }
      })
      // write(state, value) -> Unit
      // Note: In JVM, this is a side effect for runtime
      .add(packageName, "write", FfiCall.Fn2 { (state, value) =>
        state.asExternal.toAny match {
          case s: UIState[Value @unchecked] => s.value = value
          case _ => ()
        }
        UnitValue
      })
      // on_click(handler) -> (String, String)
      .add(packageName, "on_click", FfiCall.Fn1 { handler =>
        // Return a tuple marking this as a click handler
        // The handler ID is embedded in the value
        val handlerId = s"handler_${System.identityHashCode(handler)}"
        ProductValue(Array(Str("data-onclick"), Str(handlerId)))
      })
      // on_input(handler) -> (String, String)
      .add(packageName, "on_input", FfiCall.Fn1 { handler =>
        val handlerId = s"handler_${System.identityHashCode(handler)}"
        ProductValue(Array(Str("data-oninput"), Str(handlerId)))
      })
      // on_change(handler) -> (String, String)
      .add(packageName, "on_change", FfiCall.Fn1 { handler =>
        val handlerId = s"handler_${System.identityHashCode(handler)}"
        ProductValue(Array(Str("data-onchange"), Str(handlerId)))
      })
      // list_state(initial) -> ListState[a]
      .add(packageName, "list_state", FfiCall.Fn1 { initial =>
        stateCounter += 1
        val items = VList.unapply(initial).getOrElse(Nil)
        ExternalValue(UIListState(s"list_state_$stateCounter", items))
      })
      // list_read(ls) -> List[a]
      .add(packageName, "list_read", FfiCall.Fn1 { ls =>
        ls.asExternal.toAny match {
          case UIListState(_, items: List[Value] @unchecked) => VList(items)
          case _ => VList(Nil)
        }
      })
      // list_append(ls, item) -> Unit
      .add(packageName, "list_append", FfiCall.Fn2 { (ls, item) =>
        ls.asExternal.toAny match {
          case s: UIListState[Value @unchecked] => s.items = s.items :+ item
          case _ => ()
        }
        UnitValue
      })
      // list_remove_at(ls, index) -> Unit
      .add(packageName, "list_remove_at", FfiCall.Fn2 { (ls, index) =>
        ls.asExternal.toAny match {
          case s: UIListState[Value @unchecked] =>
            val idx = index match {
              case VInt(i) => BigInt(i).toInt
              case _ => -1
            }
            if (idx >= 0 && idx < s.items.length) {
              s.items = s.items.take(idx) ++ s.items.drop(idx + 1)
            }
          case _ => ()
        }
        UnitValue
      })
      // list_update_at(ls, index, item) -> Unit
      .add(packageName, "list_update_at", FfiCall.Fn3 { (ls, index, item) =>
        ls.asExternal.toAny match {
          case s: UIListState[Value @unchecked] =>
            val idx = index match {
              case VInt(i) => BigInt(i).toInt
              case _ => -1
            }
            if (idx >= 0 && idx < s.items.length) {
              s.items = s.items.updated(idx, item)
            }
          case _ => ()
        }
        UnitValue
      })
      // list_length(ls) -> Int
      .add(packageName, "list_length", FfiCall.Fn1 { ls =>
        ls.asExternal.toAny match {
          case UIListState(_, items: List[_]) => VInt(BigInt(items.length))
          case _ => VInt(BigInt(0))
        }
      })
}
