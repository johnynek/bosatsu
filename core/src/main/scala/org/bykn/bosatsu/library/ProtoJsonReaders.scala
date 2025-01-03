package org.bykn.bosatsu.library

import org.bykn.bosatsu.Json
import org.bykn.bosatsu.Json.{Reader, Writer}

import _root_.bosatsu.{TypedAst => proto}

import cats.syntax.all._

object ProtoJsonReaders {

  implicit val versionReader: Reader[Version] =
    Reader.fromParser("version", Version.parser)

  implicit val descriptorReader: Reader[proto.LibDescriptor] =
    new Reader.Obj[proto.LibDescriptor] {
      def describe: String = "LibDescriptor"
      def readObj(from: Reader.FromObj): Either[(String, Json, Json.Path), proto.LibDescriptor] = {
        for {
          version <- from.field[Version]("version")
          versionProto = version.toProto
          hashes <- from.field[List[String]]("hashes")
          uris <- from.field[List[String]]("uris")
        } yield proto.LibDescriptor(version = Some(versionProto), hashes, uris)
      }
    }

  implicit val descriptorWriter: Writer[proto.LibDescriptor] =
    Writer.from[proto.LibDescriptor] { ld =>
      Json.JObject(
        (ld.version match {
          case Some(v) => ("version" -> Json.JString(Version.fromProto(v).render)) :: Nil
          case None => Nil
        }) :::
        ("hashes" -> Writer.write(ld.hashes.toList)) ::
        ("uris" -> Writer.write(ld.uris.toList)) ::
        Nil
      )  
    }
  
  implicit val historyReader: Reader[proto.LibHistory] =
    new Reader.Obj[proto.LibHistory] {
      def describe: String = "LibHistory"
      def readObj(from: Reader.FromObj): Either[(String, Json, Json.Path), proto.LibHistory] =
        for {
          previousPatch <- from.optional[proto.LibDescriptor]("previous_patch")
          previousMinor <- from.optional[proto.LibDescriptor]("previous_minor")
          previousMajor <- from.optional[proto.LibDescriptor]("previous_major")
          previousPre <- from.optional[proto.LibDescriptor]("previous_prerelease")
          othersOpt <- from.optional[List[proto.LibDescriptor]]("others")
        } yield proto.LibHistory(
          previousPatch = previousPatch,
          previousMinor = previousMinor,
          previousMajor = previousMajor,
          previousPrerelease = previousPre,
          others = othersOpt.toList.flatten)
    }

  implicit val listOfDepsReader: Reader[List[proto.LibDependency]] =
    new Reader[List[proto.LibDependency]] {
      def describe: String = "List[LibDependency]"
      def read(path: Json.Path, j: Json): Either[(String, Json, Json.Path), List[proto.LibDependency]] =
        j match {
          case jobj: Json.JObject =>
            // use the keys as names and the values as strings
            jobj.keys.traverse { k =>
              val desc = jobj.toMap(k)  
              val p1 = path.key(k)
              Reader[proto.LibDescriptor].read(p1, desc)
                .map { desc => proto.LibDependency(name = k, desc = Some(desc))}
            }

          case _ =>
            Left(("expected obj with values of type LibDescriptor", j, path))
        }
    }

  implicit val listOfDepsWriter: Json.Writer[List[proto.LibDependency]] =
    Json.Writer { list =>
      Json.JObject(list.flatMap { dep =>
        dep.desc.map { d =>
          dep.name -> Json.Writer.write(d)
        }
      }) 
    }
}