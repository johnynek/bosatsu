package dev.bosatsu.ui

import org.scalacheck.{Arbitrary, Gen}
import dev.bosatsu.{Identifier, TypedExpr}

/**
 * ScalaCheck generators for VNode and related types.
 *
 * These generators create random VNode trees for property-based testing.
 * They are designed to:
 * - Generate valid VNode trees at various depths
 * - Create representative attribute values
 * - Support shrinking for minimal failing examples
 */
object VNodeGen {

  // HTML tag names for generation
  val commonTags: List[String] = List(
    "div", "span", "p", "a", "button", "input", "form",
    "ul", "ol", "li", "h1", "h2", "h3", "h4", "h5", "h6",
    "header", "footer", "nav", "main", "section", "article",
    "table", "tr", "td", "th", "thead", "tbody",
    "img", "video", "audio", "canvas", "svg"
  )

  val voidTags: List[String] = List(
    "area", "base", "br", "col", "embed", "hr", "img", "input",
    "link", "meta", "param", "source", "track", "wbr"
  )

  // Attribute names for generation
  val commonAttributes: List[String] = List(
    "id", "class", "style", "title", "data-testid",
    "href", "src", "alt", "type", "name", "value",
    "placeholder", "disabled", "readonly", "required"
  )

  val eventTypes: List[String] = List(
    "click", "dblclick", "mouseenter", "mouseleave",
    "keydown", "keyup", "keypress",
    "input", "change", "submit", "focus", "blur"
  )

  // Generator for valid CSS class names
  val genClassName: Gen[String] = for {
    first <- Gen.alphaChar
    len <- Gen.choose(0, 15)
    rest <- Gen.listOfN(len, Gen.alphaNumChar)
  } yield (first :: rest).mkString

  // Generator for valid HTML id
  val genId: Gen[String] = for {
    first <- Gen.alphaChar
    len <- Gen.choose(0, 10)
    rest <- Gen.listOfN(len, Gen.alphaNumChar)
  } yield (first :: rest).mkString

  // Generator for attribute values
  val genAttributeValue: Gen[AttributeValue] = Gen.oneOf(
    Gen.alphaNumStr.map(AttributeValue.StringValue.apply),
    Gen.chooseNum(-1000, 1000).map(AttributeValue.IntValue.apply),
    Gen.double.map(AttributeValue.DoubleValue.apply),
    Gen.oneOf(true, false).map(AttributeValue.BoolValue.apply),
    Gen.const(AttributeValue.NullValue)
  )

  // Generator for string attribute values (most common)
  val genStringAttribute: Gen[AttributeValue.StringValue] =
    Gen.alphaNumStr.map(AttributeValue.StringValue.apply)

  // Generator for attributes map
  def genAttributes(maxAttrs: Int = 5): Gen[Map[String, AttributeValue]] = for {
    numAttrs <- Gen.choose(0, maxAttrs)
    attrNames <- Gen.listOfN(numAttrs, Gen.oneOf(commonAttributes)).map(_.distinct)
    attrValues <- Gen.listOfN(attrNames.length, genAttributeValue)
  } yield attrNames.zip(attrValues).toMap

  // Generator for text content (safe for HTML)
  val genTextContent: Gen[String] = for {
    len <- Gen.choose(0, 50)
    chars <- Gen.listOfN(len, Gen.frequency(
      (10, Gen.alphaNumChar),
      (2, Gen.const(' ')),
      (1, Gen.oneOf('.', ',', '!', '?', '-'))
    ))
  } yield chars.mkString

  // Generator for Text nodes
  val genTextNode: Gen[VNode.Text] =
    genTextContent.map(VNode.Text.apply)

  // Generator for Element nodes (leaf - no children)
  val genLeafElement: Gen[VNode.Element] = for {
    tag <- Gen.oneOf(commonTags)
    attrs <- genAttributes()
  } yield VNode.Element(tag, attrs, Map.empty, Nil, None)

  // Generator for VNode keys
  val genKey: Gen[Option[String]] = Gen.option(
    Gen.listOfN(8, Gen.alphaNumChar).map(_.mkString)
  )

  // Recursive generator for VNode trees with depth control
  def genVNode(maxDepth: Int = 3): Gen[VNode] = {
    if (maxDepth <= 0) {
      // Base case: only text or leaf elements
      Gen.oneOf(genTextNode, genLeafElement)
    } else {
      Gen.frequency(
        (3, genTextNode),
        (2, genLeafElement),
        (5, genElementWithChildren(maxDepth - 1))
      )
    }
  }

  // Generator for elements with children
  def genElementWithChildren(childDepth: Int): Gen[VNode.Element] = for {
    tag <- Gen.oneOf(commonTags.filterNot(voidTags.contains))
    attrs <- genAttributes()
    numChildren <- Gen.choose(0, 5)
    children <- Gen.listOfN(numChildren, genVNode(childDepth))
    key <- genKey
  } yield VNode.Element(tag, attrs, Map.empty, children, key)

  // Generator for shallow VNode trees (depth 1)
  val genShallowVNode: Gen[VNode] = genVNode(1)

  // Generator for medium VNode trees (depth 2)
  val genMediumVNode: Gen[VNode] = genVNode(2)

  // Generator for deep VNode trees (depth 3)
  val genDeepVNode: Gen[VNode] = genVNode(3)

  // Generator for VNode lists (e.g., for testing list rendering)
  def genVNodeList(minSize: Int = 0, maxSize: Int = 10): Gen[List[VNode]] = for {
    size <- Gen.choose(minSize, maxSize)
    nodes <- Gen.listOfN(size, genVNode(2))
  } yield nodes

  // Generator for keyed VNode lists
  def genKeyedVNodeList(size: Int): Gen[List[VNode.Element]] = for {
    keys <- Gen.listOfN(size, genKey).map(_.flatten.distinct)
    elements <- Gen.listOfN(keys.length, genLeafElement)
  } yield elements.zip(keys).map { case (el, key) =>
    el.copy(key = Some(key))
  }

  // Arbitrary instances for ScalaCheck
  implicit val arbAttributeValue: Arbitrary[AttributeValue] =
    Arbitrary(genAttributeValue)

  implicit val arbTextNode: Arbitrary[VNode.Text] =
    Arbitrary(genTextNode)

  implicit val arbElement: Arbitrary[VNode.Element] =
    Arbitrary(genElementWithChildren(2))

  implicit val arbVNode: Arbitrary[VNode] =
    Arbitrary(genVNode(3))

  // Helper to calculate VNode tree depth
  def depth(vnode: VNode): Int = vnode match {
    case VNode.Text(_) => 1
    case VNode.Element(_, _, _, children, _) =>
      if (children.isEmpty) 1 else 1 + children.map(depth).max
    case VNode.Component(_, _, render, _) => 1 + depth(render())
  }

  // Helper to count total nodes in a VNode tree
  def nodeCount(vnode: VNode): Int = vnode match {
    case VNode.Text(_) => 1
    case VNode.Element(_, _, _, children, _) => 1 + children.map(nodeCount).sum
    case VNode.Component(_, _, render, _) => 1 + nodeCount(render())
  }
}
