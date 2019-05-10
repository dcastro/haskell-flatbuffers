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

                case "VectorOfUnions" =>
                  val obj = VectorOfUnions.getRootAsVectorOfUnions(bb)
                  Json.obj(
                    "xs" =>>
                      (0 until obj.xsLength()).map { i =>
                        readWeapon(obj)(_.xsType(i), root => union => root.xs(union, i))
                      },
                    "xsReq" =>>
                      (0 until obj.xsReqLength()).map { i =>
                        readWeapon(obj)(_.xsReqType(i), root => union => root.xsReq(union, i))
                      }
                  ).some

                case _ => none
              }
            }

          json.flatMap(j => Try(j.map(_.noSpaces))) match {
            case Success(Some(j)) => Ok(j)
            case Success(None)    => BadRequest("Unrecognized flatbuffer name")
            case Failure(err)     =>
              BadRequest(
                Json.obj(
                  "bytes" =>> bytes.grouped(4).map(_.mkString(",")).toList,
                  "error" =>> err.toString
                ).spaces2
              )
          }
        }
    }
  }

  def inside[A](obj: A)(f: A => Json): Json =
    Option(obj) match {
      case Some(x) => f(x)
      case None    => Json.Null
    }

  implicit class BetterStringOps(value: String) {
    /** Like :=, but checks for nulls. */
    def =>>[A: Encoder](a: A): (String, Json) =
      Option(a) match {
        case Some(x) => (value, x.asJson)
        case None    => (value, Json.Null)
      }
  }

  def readWeapon[A <: Table](obj: A)(weaponType: A => Byte, weapon: A => Table => Table): Json =
    weaponType(obj) match {
      case Weapon.NONE => "NONE".asJson
      case Weapon.Sword   =>
        val uni = weapon(obj)(new Sword()).asInstanceOf[Sword]
        Json.obj("x" =>> uni.x)
      case Weapon.Axe   =>
        val uni = weapon(obj)(new Axe()).asInstanceOf[Axe]
        Json.obj("y" =>> uni.y)
    }

  val asWord64: Long => JsonNumber =
    l => JsonNumber.fromIntegralStringUnsafe(java.lang.Long.toUnsignedString(l))
}


