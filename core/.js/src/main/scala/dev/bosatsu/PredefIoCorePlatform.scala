package dev.bosatsu

import cats.data.NonEmptyList
import java.math.BigInteger
import scala.scalajs.js
import scala.scalajs.js.JavaScriptException
import scala.util.Try

private[bosatsu] object PredefIoCorePlatform {
  import Value._

  private val ProgTagEffect = 5

  private val IOErrorTagNotFound = 0
  private val IOErrorTagAccessDenied = 1
  private val IOErrorTagAlreadyExists = 2
  private val IOErrorTagNotDirectory = 3
  private val IOErrorTagIsDirectory = 4
  private val IOErrorTagNotEmpty = 5
  private val IOErrorTagTooManyOpenFiles = 6
  private val IOErrorTagReadOnlyFileSystem = 7
  private val IOErrorTagCrossDeviceLink = 8
  private val IOErrorTagNoSpace = 9
  private val IOErrorTagQuotaExceeded = 10
  private val IOErrorTagNameTooLong = 11
  private val IOErrorTagInvalidArgument = 12
  private val IOErrorTagInvalidUtf8 = 13
  private val IOErrorTagBadFileDescriptor = 14
  private val IOErrorTagInterrupted = 15
  private val IOErrorTagWouldBlock = 16
  private val IOErrorTagTimedOut = 17
  private val IOErrorTagBrokenPipe = 18
  private val IOErrorTagUnsupported = 19
  private val IOErrorTagOther = 20

  private val MaxIntBI = BigInteger.valueOf(Int.MaxValue.toLong)
  private val OneMillionBI = BigInteger.valueOf(1000000L)

  private var nodeRuntimeOverride: Option[Boolean] = None

  private sealed trait NodeHandle derives CanEqual
  private case object HandleStdin extends NodeHandle
  private case object HandleStdout extends NodeHandle
  private case object HandleStderr extends NodeHandle
  private final case class NodeFileHandle(
      fd: Int,
      readable: Boolean,
      writable: Boolean,
      var closed: Boolean = false
  ) extends NodeHandle

  private lazy val nodeFs: js.Dynamic =
    js.Dynamic.global.require("fs")
  private lazy val nodePath: js.Dynamic =
    js.Dynamic.global.require("path")
  private lazy val nodeOs: js.Dynamic =
    js.Dynamic.global.require("os")
  private lazy val nodeProcess: js.Dynamic =
    js.Dynamic.global.process
  private lazy val nodeBuffer: js.Dynamic =
    js.Dynamic.global.Buffer

  private def isNodeRuntime: Boolean =
    nodeRuntimeOverride.getOrElse {
      js.typeOf(js.Dynamic.global.process) != "undefined" &&
      js.typeOf(js.Dynamic.global.process.versions) != "undefined" &&
      js.typeOf(js.Dynamic.global.process.versions.node) != "undefined"
    }

  private def browserUnsupportedContext(op: String): String =
    s"$op requires Node.js runtime host (filesystem/process APIs are unavailable in this Scala.js runtime)"

  private def optionValue(v: Option[Value]): Value =
    v match {
      case Some(value) => VOption.some(value)
      case None        => VOption.none
    }

  private def runtimeReadBytes(count: Int): Option[Array[Byte]] =
    PredefImpl.runtimeReadBytes(count)

  private def runtimeReadUtf8Chunk(
      requestedRaw: BigInteger
  ): Option[Either[Value, String]] =
    PredefImpl.runtimeReadUtf8Chunk(requestedRaw)

  private def runtimeAppendStdout(text: String): Boolean =
    PredefImpl.runtimeAppendStdout(text)

  private def runtimeAppendStderr(text: String): Boolean =
    PredefImpl.runtimeAppendStderr(text)

  private def ioerror_known(tag: Int, context: String): Value =
    SumValue(tag, ProductValue.single(Str(context)))

  private def ioerror_invalid_argument(context: String): Value =
    ioerror_known(IOErrorTagInvalidArgument, context)

  private def ioerror_invalid_utf8(context: String): Value =
    ioerror_known(IOErrorTagInvalidUtf8, context)

  private def ioerror_other(
      context: String,
      code: Int,
      message: String
  ): Value =
    SumValue(
      IOErrorTagOther,
      ProductValue.fromList(Str(context) :: VInt(code) :: Str(message) :: Nil)
    )

  private def jsField(value: Any, field: String): Option[Any] = {
    val dynamic = value.asInstanceOf[js.Dynamic]
    val raw = dynamic.selectDynamic(field)
    if (js.isUndefined(raw) || raw == null) None
    else Some(raw.asInstanceOf[Any])
  }

  private def intFromAny(value: Any): Option[Int] =
    value match {
      case i: Int                                  => Some(i)
      case d: Double if !d.isNaN && !d.isInfinity =>
        if (d.isValidInt) Some(d.toInt)
        else None
      case s: String                               => Try(s.toInt).toOption
      case _                                       => None
    }

  private def ioerror_from_js(context: String, err: Any): Value = {
    val code = jsField(err, "code").map(_.toString)
    val errno = jsField(err, "errno").flatMap(intFromAny).getOrElse(0)
    val message = jsField(err, "message").map(_.toString).getOrElse(err.toString)

    code match {
      case Some("ENOENT")                         => ioerror_known(IOErrorTagNotFound, context)
      case Some("EACCES") | Some("EPERM")       => ioerror_known(IOErrorTagAccessDenied, context)
      case Some("EEXIST")                         => ioerror_known(IOErrorTagAlreadyExists, context)
      case Some("ENOTDIR")                        => ioerror_known(IOErrorTagNotDirectory, context)
      case Some("EISDIR")                         => ioerror_known(IOErrorTagIsDirectory, context)
      case Some("ENOTEMPTY")                      => ioerror_known(IOErrorTagNotEmpty, context)
      case Some("EMFILE") | Some("ENFILE")       => ioerror_known(IOErrorTagTooManyOpenFiles, context)
      case Some("EROFS")                          => ioerror_known(IOErrorTagReadOnlyFileSystem, context)
      case Some("EXDEV")                          => ioerror_known(IOErrorTagCrossDeviceLink, context)
      case Some("ENOSPC")                         => ioerror_known(IOErrorTagNoSpace, context)
      case Some("EDQUOT")                         => ioerror_known(IOErrorTagQuotaExceeded, context)
      case Some("ENAMETOOLONG")                   => ioerror_known(IOErrorTagNameTooLong, context)
      case Some("EINVAL")                         => ioerror_invalid_argument(context)
      case Some("EBADF")                          => ioerror_known(IOErrorTagBadFileDescriptor, context)
      case Some("EINTR")                          => ioerror_known(IOErrorTagInterrupted, context)
      case Some("EAGAIN") | Some("EWOULDBLOCK") =>
        ioerror_known(IOErrorTagWouldBlock, context)
      case Some("ETIMEDOUT")                      => ioerror_known(IOErrorTagTimedOut, context)
      case Some("EPIPE")                          => ioerror_known(IOErrorTagBrokenPipe, context)
      case Some("ENOSYS") | Some("ENOTSUP") | Some("EOPNOTSUPP") =>
        ioerror_known(IOErrorTagUnsupported, context)
      case Some("ERR_INVALID_ARG_TYPE") | Some("ERR_INVALID_ARG_VALUE") |
          Some("ERR_OUT_OF_RANGE") =>
        ioerror_invalid_argument(context)
      case Some(codeStr)                           =>
        ioerror_other(context, errno, s"$codeStr: $message")
      case None                                    =>
        ioerror_other(context, errno, message)
    }
  }

  private def ioerror_from_throwable(context: String, t: Throwable): Value =
    t match {
      case JavaScriptException(value) =>
        ioerror_from_js(context, value)
      case _: IllegalArgumentException =>
        ioerror_invalid_argument(context)
      case other =>
        ioerror_other(
          context,
          0,
          Option(other.getMessage).getOrElse(other.getClass.getSimpleName)
        )
    }

  private def prog_effect(arg: Value, fn: Value => Value): Value =
    SumValue(
      ProgTagEffect,
      ProductValue.fromList(
        arg :: FnValue { case NonEmptyList(a, _) => fn(a) } :: Nil
      )
    )

  private def prog_effect2(
      a: Value,
      b: Value,
      fn: (Value, Value) => Value
  ): Value =
    prog_effect(
      Value.Tuple(a, b),
      {
        case p: ProductValue if p.values.length == 2 =>
          fn(p.get(0), p.get(1))
        case other =>
          sys.error(s"invalid effect2 payload: $other")
      }
    )

  private def prog_effect3(
      a: Value,
      b: Value,
      c: Value,
      fn: (Value, Value, Value) => Value
  ): Value =
    prog_effect(
      Value.Tuple(a, b, c),
      {
        case p: ProductValue if p.values.length == 3 =>
          fn(p.get(0), p.get(1), p.get(2))
        case other =>
          sys.error(s"invalid effect3 payload: $other")
      }
    )

  private def unsupportedProg(op: String): Value =
    PredefImpl.prog_raise_error(
      ioerror_known(IOErrorTagUnsupported, browserUnsupportedContext(op))
    )

  private def asPathString(path: Value): Either[Value, String] =
    path match {
      case Str(s) =>
        Right(s)
      case p: ProductValue if p.values.length == 1 =>
        p.get(0) match {
          case Str(s) => Right(s)
          case _      => Left(ioerror_invalid_argument("invalid Path value"))
        }
      case _ =>
        Left(ioerror_invalid_argument("invalid Path value"))
    }

  private def asOptionPathString(
      value: Value,
      context: String
  ): Either[Value, Option[String]] =
    value match {
      case VOption(None)             => Right(None)
      case VOption(Some(pathValue))  => asPathString(pathValue).map(Some(_)).left.map(_ => ioerror_invalid_argument(context))
      case _                         => Left(ioerror_invalid_argument(context))
    }

  private def asString(value: Value, context: String): Either[Value, String] =
    value match {
      case Str(s) => Right(s)
      case _      => Left(ioerror_invalid_argument(context))
    }

  private def asOpenModeTag(v: Value, context: String): Either[Value, Int] =
    v match {
      case s: SumValue if s.variant >= 0 && s.variant <= 3 => Right(s.variant)
      case _                                               => Left(ioerror_invalid_argument(context))
    }

  private def asHandleValue(v: Value): Either[Value, NodeHandle] =
    v match {
      case ExternalValue(h: NodeHandle) => Right(h)
      case _                            => Left(ioerror_invalid_argument("expected Handle"))
    }

  private def asBool(v: Value): Either[Value, Boolean] =
    v match {
      case s: SumValue if s.variant == 1 && (s.value == UnitValue) => Right(true)
      case s: SumValue if s.variant == 0 && (s.value == UnitValue) => Right(false)
      case _ =>
        Left(ioerror_invalid_argument("expected Bool"))
    }

  private def asInt(value: Value, context: String): Either[Value, BigInteger] =
    value match {
      case VInt(i) => Right(i)
      case _       => Left(ioerror_invalid_argument(context))
    }

  private def asPositiveIntBounded(
      value: Value,
      context: String
  ): Either[Value, Int] =
    asInt(value, context).flatMap { bi =>
      if (bi.signum <= 0) Left(ioerror_invalid_argument(context))
      else if (bi.compareTo(MaxIntBI) > 0) Right(Int.MaxValue)
      else Right(bi.intValue)
    }

  private def asOptionInt(
      value: Value,
      context: String
  ): Either[Value, Option[BigInteger]] =
    value match {
      case VOption(None)          => Right(None)
      case VOption(Some(VInt(i))) => Right(Some(i))
      case _                      => Left(ioerror_invalid_argument(context))
    }

  private def asDurationNanos(v: Value): Either[Value, BigInteger] =
    v match {
      case VInt(i) => Right(i)
      case p: ProductValue if p.values.length == 1 =>
        p.get(0) match {
          case VInt(i) => Right(i)
          case _       => Left(ioerror_invalid_argument("invalid Duration value"))
        }
      case _ =>
        Left(ioerror_invalid_argument("invalid Duration value"))
    }

  private def normalizePathString(raw: String): String =
    raw.replace('\\', '/')

  private def pathValueFromString(raw: String): Value =
    Str(normalizePathString(raw))

  private def bytesToArray(bytes: PredefImpl.BytesValue): Array[Byte] = {
    val out = new Array[Byte](bytes.len)
    var idx = 0
    while (idx < bytes.len) {
      out(idx) = bytes.data(bytes.offset + idx)
      idx = idx + 1
    }
    out
  }

  private def bytesValue(bytes: PredefImpl.BytesValue): Value =
    if (bytes.len == 0) PredefImpl.emptyBytes
    else ExternalValue(bytes)

  private def bufferFromBytes(bytes: PredefImpl.BytesValue): js.Dynamic = {
    if (bytes.len == 0) nodeBuffer.alloc(0)
    else {
      val data = new js.Array[Int](bytes.len)
      var idx = 0
      while (idx < bytes.len) {
        data(idx) = bytes.data(bytes.offset + idx).toInt & 0xff
        idx = idx + 1
      }
      nodeBuffer.from(data)
    }
  }

  private def bytesFromBuffer(buffer: js.Dynamic, length: Int): PredefImpl.BytesValue = {
    val out = new Array[Byte](length)
    var idx = 0
    while (idx < length) {
      out(idx) = (buffer.readUInt8(idx).asInstanceOf[Int] & 0xff).toByte
      idx = idx + 1
    }
    PredefImpl.BytesValue(out, 0, out.length)
  }

  private def openModeName(modeTag: Int): String =
    modeTag match {
      case 0 => "Read"
      case 1 => "WriteTruncate"
      case 2 => "Append"
      case 3 => "CreateNew"
      case _ => s"Unknown($modeTag)"
    }

  private def openModePreview(modeValue: Value): String =
    modeValue match {
      case s: SumValue if s.variant >= 0 && s.variant <= 3 =>
        openModeName(s.variant)
      case s: SumValue =>
        s"Unknown(${s.variant})"
      case _ =>
        "<invalid OpenMode>"
    }

  private def stringPreview(value: Value): String =
    value match {
      case Str(s) => s
      case _      => "<invalid String>"
    }

  private def optionalPathPreview(value: Value): String =
    value match {
      case VOption(None) =>
        "<default-temp-dir>"
      case VOption(Some(pathValue)) =>
        asPathString(pathValue).toOption.getOrElse("<invalid Path>")
      case _ =>
        "<invalid Path option>"
    }

  private def resolvedTempDirPreview(dirOpt: Option[String]): String =
    dirOpt.getOrElse(nodeOs.tmpdir().asInstanceOf[String])

  private def normalizeTempPrefix(prefix: String): String = {
    val base = if (prefix.isEmpty) "tmp" else prefix
    if (base.length >= 3) base
    else base + ("_" * (3 - base.length))
  }

  private def isValidTempNamePart(part: String): Boolean =
    !part.exists(ch => ch == '/' || ch == '\\' || Character.isISOControl(ch))

  private def fileKindValue(kindTag: Int): Value =
    SumValue(kindTag, UnitValue)

  private def instantValueFromNanos(nanos: BigInteger): Value =
    // Struct-1 values are represented as identity in the evaluator.
    VInt(nanos)

  private def durationValueFromNanos(nanos: BigInteger): Value =
    // Struct-1 values are represented as identity in the evaluator.
    VInt(nanos)

  private def readNodeBytes(
      handle: NodeHandle,
      maxBytes: Int
  ): Either[Value, Option[PredefImpl.BytesValue]] =
    handle match {
      case HandleStdin =>
        runtimeReadBytes(maxBytes) match {
          case Some(read) =>
            // During runProgMain, stdin is captured in the in-memory runtime buffers.
            if (read.isEmpty) Right(None)
            else Right(Some(PredefImpl.BytesValue(read, 0, read.length)))
          case None       =>
            try {
              val buffer = nodeBuffer.alloc(maxBytes)
              val count =
                nodeFs
                  .readSync(0, buffer, 0, maxBytes, js.undefined)
                  .asInstanceOf[Int]
              if (count <= 0) Right(None)
              else Right(Some(bytesFromBuffer(buffer, count)))
            } catch {
              case t: Throwable =>
                Left(ioerror_from_throwable("reading bytes from stdin", t))
            }
        }
      case HandleStdout | HandleStderr =>
        Left(
          ioerror_known(
            IOErrorTagBadFileDescriptor,
            "reading from write-only handle"
          )
        )
      case fileHandle: NodeFileHandle =>
        if (fileHandle.closed)
          Left(ioerror_known(IOErrorTagBadFileDescriptor, "reading closed handle"))
        else if (!fileHandle.readable)
          Left(ioerror_known(IOErrorTagBadFileDescriptor, "reading from write-only handle"))
        else {
          try {
            val buffer = nodeBuffer.alloc(maxBytes)
            val count =
              nodeFs
                .readSync(fileHandle.fd, buffer, 0, maxBytes, js.undefined)
                .asInstanceOf[Int]
            if (count <= 0) Right(None)
            else Right(Some(bytesFromBuffer(buffer, count)))
          } catch {
            case t: Throwable =>
              Left(ioerror_from_throwable("reading bytes from handle", t))
          }
        }
    }

  private def writeNodeBuffer(
      handle: NodeHandle,
      buffer: js.Dynamic,
      context: String
  ): Either[Value, Unit] =
    handle match {
      case HandleStdout =>
        val asText =
          buffer
            .asInstanceOf[js.Dynamic]
            .applyDynamic("toString")("utf8")
            .asInstanceOf[String]
        if (runtimeAppendStdout(asText)) Right(())
        else {
          try {
            val _ = nodeProcess.stdout.write(buffer)
            Right(())
          } catch {
            case t: Throwable =>
              Left(ioerror_from_throwable("writing to stdout", t))
          }
        }
      case HandleStderr =>
        val asText =
          buffer
            .asInstanceOf[js.Dynamic]
            .applyDynamic("toString")("utf8")
            .asInstanceOf[String]
        if (runtimeAppendStderr(asText)) Right(())
        else {
          try {
            val _ = nodeProcess.stderr.write(buffer)
            Right(())
          } catch {
            case t: Throwable =>
              Left(ioerror_from_throwable("writing to stderr", t))
          }
        }
      case HandleStdin =>
        Left(ioerror_known(IOErrorTagBadFileDescriptor, "writing to read-only handle"))
      case fileHandle: NodeFileHandle =>
        if (fileHandle.closed)
          Left(ioerror_known(IOErrorTagBadFileDescriptor, "writing closed handle"))
        else if (!fileHandle.writable)
          Left(ioerror_known(IOErrorTagBadFileDescriptor, "writing to read-only handle"))
        else {
          val total = buffer.length.asInstanceOf[Int]
          try {
            var written = 0
            while (written < total) {
              val count =
                nodeFs
                  .writeSync(
                    fileHandle.fd,
                    buffer,
                    written,
                    total - written,
                    js.undefined
                  )
                  .asInstanceOf[Int]
              if (count <= 0) {
                return Left(ioerror_known(IOErrorTagBrokenPipe, context))
              }
              written = written + count
            }
            Right(())
          } catch {
            case t: Throwable =>
              Left(ioerror_from_throwable(context, t))
          }
        }
    }

  private def core_read_bytes_impl(
      handleValue: Value,
      maxBytesValue: Value
  ): Either[Value, Option[PredefImpl.BytesValue]] =
    for {
      handle <- asHandleValue(handleValue)
      maxBytes <- asPositiveIntBounded(
        maxBytesValue,
        "read_bytes max_bytes must be > 0"
      )
      result <- readNodeBytes(handle, maxBytes)
    } yield result

  private def core_write_bytes_impl(
      handleValue: Value,
      bytesValue0: Value
  ): Either[Value, Unit] =
    for {
      handle <- asHandleValue(handleValue)
      bytes <- bytesValue0 match {
        case ExternalValue(_: PredefImpl.BytesValue) =>
          Right(bytesValue0.asExternal.toAny.asInstanceOf[PredefImpl.BytesValue])
        case _ =>
          Left(ioerror_invalid_argument("expected Bytes for write_bytes"))
      }
      _ <- writeNodeBuffer(handle, bufferFromBytes(bytes), "writing bytes to handle")
    } yield ()

  private def decodeUtf8(bytes: PredefImpl.BytesValue): Either[Value, String] = {
    val buffer = bufferFromBytes(bytes)
    try {
      Right(
        buffer
          .asInstanceOf[js.Dynamic]
          .applyDynamic("toString")("utf8")
          .asInstanceOf[String]
      )
    }
    catch {
      case t: Throwable =>
        Left(ioerror_from_throwable("decoding bytes as utf8", t))
    }
  }

  private def core_read_utf8_impl(
      handleValue: Value,
      maxCharsValue: Value
  ): Either[Value, Option[String]] =
    for {
      handle <- asHandleValue(handleValue)
      maxCharsBI <- asInt(maxCharsValue, "expected Int for max_chars")
      maxChars <- {
        if (maxCharsBI.signum <= 0)
          Left(ioerror_invalid_argument("read_utf8 max_chars must be > 0"))
        else if (maxCharsBI.compareTo(MaxIntBI) > 0) Right(Int.MaxValue)
        else Right(maxCharsBI.intValue)
      }
      maxBytes = if (maxChars >= Int.MaxValue / 4) Int.MaxValue else maxChars * 4
      out <- handle match {
        case HandleStdin =>
          runtimeReadUtf8Chunk(maxCharsBI) match {
            case Some(result) =>
              result.map { chunk =>
                if (chunk.isEmpty) None else Some(chunk)
              }
            case None         =>
              readNodeBytes(handle, maxBytes).flatMap {
                case None        => Right(None)
                case Some(chunk) =>
                  decodeUtf8(chunk).map(Some(_)).left.map {
                    case s: SumValue if s.variant == IOErrorTagInvalidUtf8 => s
                    case _                                                 => ioerror_invalid_utf8("reading utf8 from stdin")
                  }
              }
          }
        case _ =>
          readNodeBytes(handle, maxBytes).flatMap {
            case None        => Right(None)
            case Some(chunk) =>
              decodeUtf8(chunk).map(Some(_)).left.map {
                case s: SumValue if s.variant == IOErrorTagInvalidUtf8 => s
                case _                                                 => ioerror_invalid_utf8("reading utf8 from handle")
              }
          }
      }
    } yield out

  private def core_write_utf8_impl(
      handleValue: Value,
      textValue: Value
  ): Either[Value, Unit] =
    for {
      handle <- asHandleValue(handleValue)
      text <- asString(textValue, "expected String for write_utf8")
      _ <- writeNodeBuffer(
        handle,
        nodeBuffer.from(text, "utf8"),
        "writing utf8 to handle"
      )
    } yield ()

  private def core_read_all_bytes_impl(
      handleValue: Value,
      chunkSizeValue: Value
  ): Either[Value, PredefImpl.BytesValue] =
    for {
      handle <- asHandleValue(handleValue)
      chunkSize <- asPositiveIntBounded(
        chunkSizeValue,
        "read_all_bytes chunk_size must be > 0"
      )
      result <- {
        val builder = scala.collection.mutable.ArrayBuilder.make[Byte]
        var done = false
        var err: Option[Value] = None

        while (!done && err.isEmpty) {
          readNodeBytes(handle, chunkSize) match {
            case Left(e)          => err = Some(e)
            case Right(None)      => done = true
            case Right(Some(chunk)) =>
              builder ++= bytesToArray(chunk)
          }
        }

        err match {
          case Some(e) => Left(e)
          case None    =>
            val data = builder.result()
            if (data.isEmpty) Right(PredefImpl.BytesValue(Array.emptyByteArray, 0, 0))
            else Right(PredefImpl.BytesValue(data, 0, data.length))
        }
      }
    } yield result

  private def core_copy_bytes_impl(
      srcValue: Value,
      dstValue: Value,
      chunkSizeValue: Value,
      maxTotalValue: Value
  ): Either[Value, BigInteger] =
    for {
      src <- asHandleValue(srcValue)
      dst <- asHandleValue(dstValue)
      chunkSize <- asPositiveIntBounded(
        chunkSizeValue,
        "copy_bytes chunk_size must be > 0"
      )
      maxTotal <- asOptionInt(
        maxTotalValue,
        "copy_bytes max_total must be Option[Int]"
      )
      _ <- maxTotal match {
        case Some(i) if i.signum < 0 =>
          Left(ioerror_invalid_argument("copy_bytes max_total must be >= 0"))
        case _                        => Right(())
      }
      copied <- {
        maxTotal match {
          case Some(i) if i.signum == 0 =>
            Right(BigInteger.ZERO)
          case _                        =>
            var done = false
            var total = BigInteger.ZERO
            var err: Option[Value] = None

            while (!done && err.isEmpty) {
              val toRead =
                maxTotal match {
                  case Some(limit) =>
                    val remaining = limit.subtract(total)
                    if (remaining.signum <= 0) {
                      done = true
                      0
                    } else {
                      val maxChunk = BigInteger.valueOf(chunkSize.toLong)
                      if (remaining.compareTo(maxChunk) >= 0) chunkSize
                      else remaining.intValue
                    }
                  case None => chunkSize
                }

              if (!done && toRead > 0) {
                readNodeBytes(src, toRead) match {
                  case Left(e)          => err = Some(e)
                  case Right(None)      => done = true
                  case Right(Some(chunk)) =>
                    writeNodeBuffer(
                      dst,
                      bufferFromBytes(chunk),
                      "copying bytes"
                    ) match {
                      case Left(e)  => err = Some(e)
                      case Right(_) =>
                        total = total.add(BigInteger.valueOf(chunk.len.toLong))
                    }
                }
              }
            }

            err match {
              case Some(e) => Left(e)
              case None    => Right(total)
            }
        }
      }
    } yield copied

  private def core_flush_impl(handleValue: Value): Either[Value, Unit] =
    for {
      handle <- asHandleValue(handleValue)
      _ <- handle match {
        case HandleStdin | HandleStdout | HandleStderr =>
          Right(())
        case fileHandle: NodeFileHandle =>
          if (fileHandle.closed)
            Left(ioerror_known(IOErrorTagBadFileDescriptor, "flushing closed handle"))
          else if (!fileHandle.writable) Right(())
          else {
            try {
              val _ = nodeFs.fsyncSync(fileHandle.fd)
              Right(())
            } catch {
              case t: Throwable =>
                Left(ioerror_from_throwable("flushing handle", t))
            }
          }
      }
    } yield ()

  private def core_close_impl(handleValue: Value): Either[Value, Unit] =
    for {
      handle <- asHandleValue(handleValue)
      _ <- handle match {
        case HandleStdin | HandleStdout | HandleStderr =>
          Right(())
        case fileHandle: NodeFileHandle =>
          if (fileHandle.closed) Right(())
          else {
            try {
              val _ = nodeFs.closeSync(fileHandle.fd)
              fileHandle.closed = true
              Right(())
            } catch {
              case t: Throwable =>
                Left(ioerror_from_throwable("closing handle", t))
            }
          }
      }
    } yield ()

  private def core_open_file_impl(path: Value, mode: Value): Either[Value, Value] = {
    val invalidPathContext =
      s"open_file(path=<invalid Path>, mode=${openModePreview(mode)}): invalid path"

    for {
      pathStr <- asPathString(path).left.map(_ => ioerror_invalid_argument(invalidPathContext))
      modeTag <- asOpenModeTag(
        mode,
        s"open_file(path=$pathStr, mode=<invalid OpenMode>): invalid OpenMode value"
      )
      out <- {
        val openContext =
          s"open_file(path=$pathStr, mode=${openModeName(modeTag)})"
        val flags =
          modeTag match {
            case 0 => "r"
            case 1 => "w"
            case 2 => "a"
            case 3 => "wx"
            case _ => ""
          }
        if (flags.isEmpty)
          Left(ioerror_invalid_argument(s"$openContext: invalid OpenMode value"))
        else {
          try {
            val fd = nodeFs.openSync(pathStr, flags).asInstanceOf[Int]
            val handle =
              if (modeTag == 0) NodeFileHandle(fd, readable = true, writable = false)
              else NodeFileHandle(fd, readable = false, writable = true)
            Right(ExternalValue(handle))
          } catch {
            case t: Throwable =>
              Left(ioerror_from_throwable(s"$openContext: opening file failed", t))
          }
        }
      }
    } yield out
  }

  private def randomTempChunk(): String = {
    val raw = js.Math.floor(js.Math.random() * 2176782336d).toLong
    java.lang.Long.toUnsignedString(raw, 36)
  }

  private def createTempFileNode(
      dirOpt: Option[String],
      rawPrefix: String,
      rawSuffix: String,
      callContext: String
  ): Either[Value, Value] = {
    val baseDir = dirOpt.getOrElse(nodeOs.tmpdir().asInstanceOf[String])
    val prefix = normalizeTempPrefix(rawPrefix)
    var attempts = 0

    while (attempts < 128) {
      val candidateName = s"${prefix}${randomTempChunk()}$rawSuffix"
      val candidatePath =
        nodePath.join(baseDir, candidateName).asInstanceOf[String]
      try {
        val fd = nodeFs.openSync(candidatePath, "wx").asInstanceOf[Int]
        val pathOut = pathValueFromString(candidatePath)
        val handleOut =
          ExternalValue(NodeFileHandle(fd, readable = false, writable = true))
        return Right(Value.Tuple(pathOut, handleOut))
      } catch {
        case t: Throwable =>
          val err =
            ioerror_from_throwable(
              s"$callContext: Files.createTempFile failed",
              t
            )
          err match {
            case s: SumValue if s.variant == IOErrorTagAlreadyExists =>
              attempts = attempts + 1
            case _                                                   =>
              return Left(err)
          }
      }
    }

    Left(
      ioerror_other(
        s"$callContext: Files.createTempFile failed",
        0,
        "unable to create unique temp file"
      )
    )
  }

  private def core_create_temp_file_impl(
      dir: Value,
      prefix: Value,
      suffix: Value
  ): Either[Value, Value] = {
    val prefixPreview = stringPreview(prefix)
    val suffixPreview = stringPreview(suffix)
    val dirPreview = optionalPathPreview(dir)

    for {
      dirOpt <- asOptionPathString(
        dir,
        s"create_temp_file(dir=<invalid Path option>, prefix=$prefixPreview, suffix=$suffixPreview): invalid temp file dir"
      )
      rawPrefix <- asString(
        prefix,
        s"create_temp_file(dir=$dirPreview, prefix=<invalid String>, suffix=$suffixPreview): invalid temp file prefix"
      )
      rawSuffix <- asString(
        suffix,
        s"create_temp_file(dir=$dirPreview, prefix=$rawPrefix, suffix=<invalid String>): invalid temp file suffix"
      )
      callContext =
        s"create_temp_file(dir=${resolvedTempDirPreview(dirOpt)}, prefix=$rawPrefix, suffix=$rawSuffix)"
      _ <-
        if (isValidTempNamePart(rawPrefix)) Right(())
        else Left(ioerror_invalid_argument(s"$callContext: invalid temp file prefix"))
      _ <-
        if (isValidTempNamePart(rawSuffix)) Right(())
        else Left(ioerror_invalid_argument(s"$callContext: invalid temp file suffix"))
      tempFile <- createTempFileNode(dirOpt, rawPrefix, rawSuffix, callContext)
    } yield tempFile
  }

  private def core_create_temp_dir_impl(
      dir: Value,
      prefix: Value
  ): Either[Value, Value] = {
    val prefixPreview = stringPreview(prefix)
    val dirPreview = optionalPathPreview(dir)

    for {
      dirOpt <- asOptionPathString(
        dir,
        s"create_temp_dir(dir=<invalid Path option>, prefix=$prefixPreview): invalid temp dir"
      )
      rawPrefix <- asString(
        prefix,
        s"create_temp_dir(dir=$dirPreview, prefix=<invalid String>): invalid temp dir prefix"
      )
      callContext = s"create_temp_dir(dir=${resolvedTempDirPreview(dirOpt)}, prefix=$rawPrefix)"
      _ <-
        if (isValidTempNamePart(rawPrefix)) Right(())
        else Left(ioerror_invalid_argument(s"$callContext: invalid temp dir prefix"))
      out <- {
        val base = dirOpt.getOrElse(nodeOs.tmpdir().asInstanceOf[String])
        val seed = nodePath.join(base, normalizeTempPrefix(rawPrefix)).asInstanceOf[String]
        try {
          val created = nodeFs.mkdtempSync(seed).asInstanceOf[String]
          Right(pathValueFromString(created))
        } catch {
          case t: Throwable =>
            Left(ioerror_from_throwable(s"$callContext: Files.createTempDirectory failed", t))
        }
      }
    } yield out
  }

  private def core_list_dir_impl(path: Value): Either[Value, Value] =
    for {
      pathStr <- asPathString(path).left.map(_ => ioerror_invalid_argument("invalid path for list_dir"))
      entries <- {
        try {
          val arr = nodeFs.readdirSync(pathStr).asInstanceOf[js.Array[Any]]
          val list = arr.iterator
            .map(_.toString)
            .map(name => normalizePathString(nodePath.join(pathStr, name).asInstanceOf[String]))
            .toList
            .sorted
            // Path is a struct-1 and represented as identity at runtime.
            .map(Str(_))
          Right(Value.VList(list))
        } catch {
          case t: Throwable =>
            Left(ioerror_from_throwable("list_dir", t))
        }
      }
    } yield entries

  private def core_stat_impl(path: Value): Either[Value, Value] =
    for {
      pathStr <- asPathString(path).left.map(_ => ioerror_invalid_argument("invalid path for stat"))
      stat <- {
        try {
          val attrs = nodeFs.lstatSync(pathStr)
          val isSym = attrs.isSymbolicLink().asInstanceOf[Boolean]
          val isFile = attrs.isFile().asInstanceOf[Boolean]
          val isDir = attrs.isDirectory().asInstanceOf[Boolean]
          val kindTag = if (isSym) 2 else if (isFile) 0 else if (isDir) 1 else 3
          val sizeBI = BigInteger.valueOf(attrs.size().asInstanceOf[Double].toLong)
          val mtimeNanos =
            BigInteger.valueOf(
              js.Math.floor(attrs.mtimeMs().asInstanceOf[Double] * 1000000d).toLong
            )
          val statValue =
            ProductValue.fromList(
              fileKindValue(kindTag) ::
                VInt(sizeBI) ::
                instantValueFromNanos(mtimeNanos) :: Nil
            )
          Right(optionValue(Some(statValue)))
        } catch {
          case t: Throwable =>
            val err = ioerror_from_throwable("stat", t)
            err match {
              case s: SumValue if s.variant == IOErrorTagNotFound =>
                Right(optionValue(None))
              case _                                              =>
                Left(err)
            }
        }
      }
    } yield stat

  private def core_mkdir_impl(path: Value, recursive: Value): Either[Value, Unit] =
    for {
      pathStr <- asPathString(path).left.map(_ => ioerror_invalid_argument("invalid path for mkdir"))
      recursiveFlag <- asBool(recursive)
      _ <- {
        try {
          val opts = js.Dynamic.literal(recursive = recursiveFlag)
          val _ = nodeFs.mkdirSync(pathStr, opts)
          Right(())
        } catch {
          case t: Throwable =>
            Left(ioerror_from_throwable("mkdir", t))
        }
      }
    } yield ()

  private def core_remove_impl(path: Value, recursive: Value): Either[Value, Unit] =
    for {
      pathStr <- asPathString(path).left.map(_ => ioerror_invalid_argument("invalid path for remove"))
      recursiveFlag <- asBool(recursive)
      _ <- {
        try {
          val opts = js.Dynamic.literal(recursive = recursiveFlag, force = false)
          val _ = nodeFs.rmSync(pathStr, opts)
          Right(())
        } catch {
          case t: Throwable =>
            Left(ioerror_from_throwable("remove", t))
        }
      }
    } yield ()

  private def core_rename_impl(from: Value, to: Value): Either[Value, Unit] =
    for {
      fromPath <- asPathString(from).left.map(_ => ioerror_invalid_argument("invalid source path for rename"))
      toPath <- asPathString(to).left.map(_ => ioerror_invalid_argument("invalid destination path for rename"))
      _ <- {
        try {
          val _ = nodeFs.renameSync(fromPath, toPath)
          Right(())
        } catch {
          case t: Throwable =>
            Left(ioerror_from_throwable("rename", t))
        }
      }
    } yield ()

  private def core_get_env_impl(name: Value): Either[Value, Option[String]] =
    for {
      envName <- asString(name, "get_env expects String")
      value = {
        val raw = nodeProcess.env.selectDynamic(envName)
        if (js.isUndefined(raw) || raw == null) None
        else Some(raw.toString)
      }
    } yield value

  private def nowWallEpochNanos(): BigInteger =
    BigInteger.valueOf(js.Math.floor(js.Date.now() * 1000000d).toLong)

  private def nowMonoNanos(): BigInteger = {
    if (isNodeRuntime) {
      val hrtime = nodeProcess.selectDynamic("hrtime")
      val bigIntFn = hrtime.selectDynamic("bigint")
      if (!js.isUndefined(bigIntFn))
        new BigInteger(bigIntFn.apply().toString)
      else BigInteger.valueOf(js.Math.floor(js.Date.now() * 1000000d).toLong)
    } else {
      BigInteger.valueOf(js.Math.floor(js.Date.now() * 1000000d).toLong)
    }
  }

  private def sleepNanos(nanos: BigInteger): Either[Value, Unit] = {
    if (nanos.signum < 0)
      Left(ioerror_invalid_argument("sleep duration must be non-negative"))
    else {
      // The evaluator is synchronous, so sleep is implemented as a bounded spin wait.
      val millis = nanos.divide(OneMillionBI)
      val extraNanos = nanos.remainder(OneMillionBI).doubleValue()
      val totalMillis =
        if (millis.compareTo(MaxIntBI) > 0) Int.MaxValue.toDouble
        else millis.longValue().toDouble + (extraNanos / 1000000d)
      val end = js.Date.now() + totalMillis
      while (js.Date.now() < end) {
        ()
      }
      Right(())
    }
  }

  val core_path_sep: Value =
    if (isNodeRuntime) Str(nodePath.sep.asInstanceOf[String])
    else Str("/")

  val core_stdin: Value = ExternalValue(HandleStdin)
  val core_stdout: Value = ExternalValue(HandleStdout)
  val core_stderr: Value = ExternalValue(HandleStderr)

  def prog_core_read_utf8(handle: Value, maxChars: Value): Value =
    if (!isNodeRuntime) unsupportedProg("read_utf8")
    else
      prog_effect2(
        handle,
        maxChars,
        (h, n) =>
          core_read_utf8_impl(h, n) match {
            case Right(result) =>
              PredefImpl.prog_pure(optionValue(result.map(Str(_))))
            case Left(err)     =>
              PredefImpl.prog_raise_error(err)
          }
      )

  def prog_core_write_utf8(handle: Value, text: Value): Value =
    if (!isNodeRuntime) unsupportedProg("write_utf8")
    else
      prog_effect2(
        handle,
        text,
        (h, t) =>
          core_write_utf8_impl(h, t) match {
            case Right(_)  => PredefImpl.prog_pure(UnitValue)
            case Left(err) => PredefImpl.prog_raise_error(err)
          }
      )

  def prog_core_read_bytes(handle: Value, maxBytes: Value): Value =
    if (!isNodeRuntime) unsupportedProg("read_bytes")
    else
      prog_effect2(
        handle,
        maxBytes,
        (h, n) =>
          core_read_bytes_impl(h, n) match {
            case Right(result) =>
              PredefImpl.prog_pure(optionValue(result.map(bytesValue)))
            case Left(err)     =>
              PredefImpl.prog_raise_error(err)
          }
      )

  def prog_core_write_bytes(handle: Value, bytes: Value): Value =
    if (!isNodeRuntime) unsupportedProg("write_bytes")
    else
      prog_effect2(
        handle,
        bytes,
        (h, b) =>
          core_write_bytes_impl(h, b) match {
            case Right(_)  => PredefImpl.prog_pure(UnitValue)
            case Left(err) => PredefImpl.prog_raise_error(err)
          }
      )

  def prog_core_read_all_bytes(handle: Value, chunkSize: Value): Value =
    if (!isNodeRuntime) unsupportedProg("read_all_bytes")
    else
      prog_effect2(
        handle,
        chunkSize,
        (h, c) =>
          core_read_all_bytes_impl(h, c) match {
            case Right(result) =>
              PredefImpl.prog_pure(bytesValue(result))
            case Left(err)     =>
              PredefImpl.prog_raise_error(err)
          }
      )

  def prog_core_copy_bytes(
      src: Value,
      dst: Value,
      chunkSize: Value,
      maxTotal: Value
  ): Value =
    if (!isNodeRuntime) unsupportedProg("copy_bytes")
    else
      prog_effect(
        Value.Tuple(src, dst, chunkSize, maxTotal),
        {
          case p: ProductValue if p.values.length == 4 =>
            core_copy_bytes_impl(p.get(0), p.get(1), p.get(2), p.get(3)) match {
              case Right(count) => PredefImpl.prog_pure(VInt(count))
              case Left(err)    => PredefImpl.prog_raise_error(err)
            }
          case other =>
            sys.error(s"invalid effect payload for copy_bytes: $other")
        }
      )

  def prog_core_flush(handle: Value): Value =
    if (!isNodeRuntime) unsupportedProg("flush")
    else
      prog_effect(
        handle,
        h =>
          core_flush_impl(h) match {
            case Right(_)  => PredefImpl.prog_pure(UnitValue)
            case Left(err) => PredefImpl.prog_raise_error(err)
          }
      )

  def prog_core_close(handle: Value): Value =
    if (!isNodeRuntime) unsupportedProg("close")
    else
      prog_effect(
        handle,
        h =>
          core_close_impl(h) match {
            case Right(_)  => PredefImpl.prog_pure(UnitValue)
            case Left(err) => PredefImpl.prog_raise_error(err)
          }
      )

  def prog_core_open_file(path: Value, mode: Value): Value =
    if (!isNodeRuntime) unsupportedProg("open_file")
    else
      prog_effect2(
        path,
        mode,
        (p, m) =>
          core_open_file_impl(p, m) match {
            case Right(handleOut) => PredefImpl.prog_pure(handleOut)
            case Left(err)        => PredefImpl.prog_raise_error(err)
          }
      )

  def prog_core_create_temp_file(
      dir: Value,
      prefix: Value,
      suffix: Value
  ): Value =
    if (!isNodeRuntime) unsupportedProg("create_temp_file")
    else
      prog_effect3(
        dir,
        prefix,
        suffix,
        (d, p, s) =>
          core_create_temp_file_impl(d, p, s) match {
            case Right(tempFile) => PredefImpl.prog_pure(tempFile)
            case Left(err)       => PredefImpl.prog_raise_error(err)
          }
      )

  def prog_core_create_temp_dir(dir: Value, prefix: Value): Value =
    if (!isNodeRuntime) unsupportedProg("create_temp_dir")
    else
      prog_effect2(
        dir,
        prefix,
        (d, p) =>
          core_create_temp_dir_impl(d, p) match {
            case Right(pathOut) => PredefImpl.prog_pure(pathOut)
            case Left(err)      => PredefImpl.prog_raise_error(err)
          }
      )

  def prog_core_list_dir(path: Value): Value =
    if (!isNodeRuntime) unsupportedProg("list_dir")
    else
      prog_effect(
        path,
        p =>
          core_list_dir_impl(p) match {
            case Right(paths) => PredefImpl.prog_pure(paths)
            case Left(err)    => PredefImpl.prog_raise_error(err)
          }
      )

  def prog_core_stat(path: Value): Value =
    if (!isNodeRuntime) unsupportedProg("stat")
    else
      prog_effect(
        path,
        p =>
          core_stat_impl(p) match {
            case Right(statValue) => PredefImpl.prog_pure(statValue)
            case Left(err)        => PredefImpl.prog_raise_error(err)
          }
      )

  def prog_core_mkdir(path: Value, recursive: Value): Value =
    if (!isNodeRuntime) unsupportedProg("mkdir")
    else
      prog_effect2(
        path,
        recursive,
        (p, r) =>
          core_mkdir_impl(p, r) match {
            case Right(_)  => PredefImpl.prog_pure(UnitValue)
            case Left(err) => PredefImpl.prog_raise_error(err)
          }
      )

  def prog_core_remove(path: Value, recursive: Value): Value =
    if (!isNodeRuntime) unsupportedProg("remove")
    else
      prog_effect2(
        path,
        recursive,
        (p, r) =>
          core_remove_impl(p, r) match {
            case Right(_)  => PredefImpl.prog_pure(UnitValue)
            case Left(err) => PredefImpl.prog_raise_error(err)
          }
      )

  def prog_core_rename(from: Value, to: Value): Value =
    if (!isNodeRuntime) unsupportedProg("rename")
    else
      prog_effect2(
        from,
        to,
        (f, t) =>
          core_rename_impl(f, t) match {
            case Right(_)  => PredefImpl.prog_pure(UnitValue)
            case Left(err) => PredefImpl.prog_raise_error(err)
          }
      )

  def prog_core_get_env(name: Value): Value =
    if (!isNodeRuntime) unsupportedProg("get_env")
    else
      prog_effect(
        name,
        n =>
          core_get_env_impl(n) match {
            case Right(result) => PredefImpl.prog_pure(optionValue(result.map(Str(_))))
            case Left(err)     => PredefImpl.prog_raise_error(err)
          }
      )

  def prog_core_spawn(cmd: Value, args: Value, stdio: Value): Value = {
    val _ = (cmd, args, stdio)
    // The Scala.js evaluator cannot safely provide blocking process orchestration.
    PredefImpl.prog_raise_error(
      ioerror_known(
        IOErrorTagUnsupported,
        "spawn is unsupported in the Scala.js evaluator runtime"
      )
    )
  }

  def prog_core_wait(process: Value): Value = {
    val _ = process
    // See prog_core_spawn above.
    PredefImpl.prog_raise_error(
      ioerror_known(
        IOErrorTagUnsupported,
        "wait is unsupported in the Scala.js evaluator runtime"
      )
    )
  }

  val prog_core_now_wall: Value =
    prog_effect(
      UnitValue,
      _ => PredefImpl.prog_pure(instantValueFromNanos(nowWallEpochNanos()))
    )

  val prog_core_now_mono: Value =
    prog_effect(
      UnitValue,
      _ => PredefImpl.prog_pure(durationValueFromNanos(nowMonoNanos()))
    )

  def prog_core_sleep(duration: Value): Value =
    prog_effect(
      duration,
      d =>
        asDurationNanos(d) match {
          case Left(err) =>
            PredefImpl.prog_raise_error(err)
          case Right(nanos) =>
            sleepNanos(nanos) match {
              case Right(_)  => PredefImpl.prog_pure(UnitValue)
              case Left(err) => PredefImpl.prog_raise_error(err)
            }
        }
    )

  def addIoCoreExternals(
      externals: Externals,
      ioCorePackageName: PackageName
  ): Externals =
    externals
      .add(ioCorePackageName, "path_sep", FfiCall.Const(core_path_sep))
      .add(ioCorePackageName, "stdin", FfiCall.Const(core_stdin))
      .add(ioCorePackageName, "stdout", FfiCall.Const(core_stdout))
      .add(ioCorePackageName, "stderr", FfiCall.Const(core_stderr))
      .add(ioCorePackageName, "read_utf8", FfiCall.Fn2(prog_core_read_utf8(_, _)))
      .add(ioCorePackageName, "write_utf8", FfiCall.Fn2(prog_core_write_utf8(_, _)))
      .add(ioCorePackageName, "read_bytes", FfiCall.Fn2(prog_core_read_bytes(_, _)))
      .add(ioCorePackageName, "write_bytes", FfiCall.Fn2(prog_core_write_bytes(_, _)))
      .add(ioCorePackageName, "read_all_bytes", FfiCall.Fn2(prog_core_read_all_bytes(_, _)))
      .add(
        ioCorePackageName,
        "copy_bytes",
        FfiCall.Fn4(prog_core_copy_bytes(_, _, _, _))
      )
      .add(ioCorePackageName, "flush", FfiCall.Fn1(prog_core_flush(_)))
      .add(ioCorePackageName, "close", FfiCall.Fn1(prog_core_close(_)))
      .add(ioCorePackageName, "open_file", FfiCall.Fn2(prog_core_open_file(_, _)))
      .add(
        ioCorePackageName,
        "create_temp_file",
        FfiCall.Fn3(prog_core_create_temp_file(_, _, _))
      )
      .add(
        ioCorePackageName,
        "create_temp_dir",
        FfiCall.Fn2(prog_core_create_temp_dir(_, _))
      )
      .add(ioCorePackageName, "list_dir", FfiCall.Fn1(prog_core_list_dir(_)))
      .add(ioCorePackageName, "stat", FfiCall.Fn1(prog_core_stat(_)))
      .add(ioCorePackageName, "mkdir", FfiCall.Fn2(prog_core_mkdir(_, _)))
      .add(ioCorePackageName, "remove", FfiCall.Fn2(prog_core_remove(_, _)))
      .add(ioCorePackageName, "rename", FfiCall.Fn2(prog_core_rename(_, _)))
      .add(ioCorePackageName, "get_env", FfiCall.Fn1(prog_core_get_env(_)))
      .add(ioCorePackageName, "spawn", FfiCall.Fn3(prog_core_spawn(_, _, _)))
      .add(ioCorePackageName, "wait", FfiCall.Fn1(prog_core_wait(_)))
      .add(ioCorePackageName, "now_wall", FfiCall.Const(prog_core_now_wall))
      .add(ioCorePackageName, "now_mono", FfiCall.Const(prog_core_now_mono))
      .add(ioCorePackageName, "sleep", FfiCall.Fn1(prog_core_sleep(_)))

  private[bosatsu] def withNodeRuntimeForTest[A](isNode: Boolean)(
      fn: => A
  ): A = {
    val old = nodeRuntimeOverride
    nodeRuntimeOverride = Some(isNode)
    try fn
    finally nodeRuntimeOverride = old
  }
}
