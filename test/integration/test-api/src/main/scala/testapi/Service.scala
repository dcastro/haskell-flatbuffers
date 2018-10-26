package testapi

import java.nio.ByteBuffer
import cats.implicits._
import cats.effect.Effect
import com.google.flatbuffers.Table
import io.circe._
import io.circe.syntax._
import org.http4s.HttpService
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import testapi.flatbuffers._
import scala.util._

class Service[F[_]: Effect] extends Http4sDsl[F] {

  val service: HttpService[F] = {
    HttpService[F] {
      case req @ POST -> Root / flatBufferName =>
        req.as[Array[Byte]].flatMap { bytes =>

          val bb = ByteBuffer.wrap(bytes)

          val json: Try[Option[Json]] =
            Try {
              flatBufferName match {
                case "Simple"     =>
                  val obj = Simple.getRootAsSimple(bb)
                  Json.obj(
                    "n" := obj.n,
                    "s" := obj.s
                  ).some

                case "FiveFields" =>
                  val obj = FiveFields.getRootAsFiveFields(bb)
                  Json.obj(
                    "n1" := obj.n1(),
                    "s1" := obj.s1(),
                    "n2" := obj.n2(),
                    "s2" := obj.s2(),
                    "n3" := obj.n3()
                  ).some

                case "ManyTables" =>
                  val obj = ManyTables.getRootAsManyTables(bb)
                  Json.obj(
                    "n" := obj.n,
                    "x" := Json.obj(
                      "n" := obj.x.n,
                      "s" := obj.x.s
                    ),
                    "y" := Json.obj(
                      "n" := obj.y.n,
                      "s" := obj.y.s
                    )
                  ).some

                case "UnionByteBool" =>
                  val obj = UnionByteBool.getRootAsUnionByteBool(bb)
                  Json.obj(
                    "color" := Color.name(obj.color.toInt),
                    "uni1" := readUnion(obj)(_.uni1Type, _.uni1),
                    "uni2" := readUnion(obj)(_.uni2Type, _.uni2),
                    "uni3" := readUnion(obj)(_.uni3Type, _.uni3),
                    "boo" := obj.boo()
                  ).some

                case "Vectors" =>
                  val obj = Vectors.getRootAsVectors(bb)
                  Json.obj(
                    "x" := Json.fromValues((0 until obj.xLength()).map(obj.x).map(_.asJson)),
                    "y" := Json.fromValues((0 until obj.yLength()).map(obj.y).map(_.asJson)),
                    "z" := Json.fromValues((0 until obj.zLength()).map(obj.z).map(_.asJson))
                  ).some

                case "Structs" =>
                  val obj = Structs.getRootAsStructs(bb)
                  Json.obj(
                    "x" := Json.obj(
                      "x" := obj.x.x,
                      "y" := obj.x.y
                    ),
                    "y" := Json.obj(
                      "w" := obj.y.w,
                      "x" := obj.y.x,
                      "y" := obj.y.y,
                      "z" := obj.y.z,
                    ),
                    "z" := Json.obj(
                      "x" := Json.obj(
                        "x" := obj.z.x.x,
                        "y" := obj.z.x.y
                      ),
                      "y" := Json.obj(
                        "w" := obj.z.y.w,
                        "x" := obj.z.y.x,
                        "y" := obj.z.y.y,
                        "z" := obj.z.y.z,
                      )
                    )
                  ).some

                case _ => none
              }
            }

          json match {
            case Success(Some(j)) => Ok(j)
            case Success(None)    => BadRequest("Unrecognized flatbuffer name")
            case Failure(err)     =>
              BadRequest(
                Json.obj(
                  "bytes" := bytes.grouped(4).map(_.mkString(",")).toList,
                  "error" := err.toString
                ).spaces2
              )
          }
        }
    }
  }

  def readUnion[A <: Table](obj: A)(unionType: A => Byte, union: A => Table => Table): Json =
    unionType(obj) match {
      case U.NONE => "NONE".asJson
      case U.UA =>
        val uni = union(obj)(new UA()).asInstanceOf[UA]
        Json.obj("x" := uni.x)
      case U.UB =>
        val uni = union(obj)(new UB()).asInstanceOf[UB]
        Json.obj("y" := uni.y)
    }
}


