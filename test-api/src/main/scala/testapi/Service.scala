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
                case "Simple" =>
                  val obj = Simple.getRootAsSimple(bb)
                  Json.obj(
                    "n" =>> obj.n,
                    "s" =>> obj.s
                  ).some

                case "Primitives" =>

                  if (!Primitives.PrimitivesBufferHasIdentifier(bb)) {
                    throw new Exception("buffer did not have expected identifier")
                  }
                  else {
                    val obj = Primitives.getRootAsPrimitives(bb)
                    Json.obj(
                      "a" =>> obj.a,
                      "b" =>> obj.b,
                      "c" =>> obj.c,
                      "d" =>> JsonNumber.fromIntegralStringUnsafe(java.lang.Long.toUnsignedString(obj.d)),
                      "e" =>> obj.e,
                      "f" =>> obj.f,
                      "g" =>> obj.g,
                      "h" =>> obj.h,
                      "i" =>> obj.i,
                      "j" =>> obj.j,
                      "k" =>> obj.k,
                      "l" =>> obj.l,
                    ).some
                  }

                case "ManyTables" =>
                  val obj = ManyTables.getRootAsManyTables(bb)
                  Json.obj(
                    "n" =>> obj.n,
                    "x" =>> inside(obj.x) { x =>
                      Json.obj(
                        "n" =>> x.n,
                        "s" =>> x.s
                      )
                    },
                    "y" =>> inside(obj.y) { y =>
                      Json.obj(
                        "n" =>> y.n,
                        "s" =>> y.s
                      )
                    },
                    "z" =>> inside(obj.z) { z =>
                      Json.obj(
                        "n" =>> z.n,
                        "s" =>> z.s
                      )
                    }
                  ).some

                case "Enums" =>
                  def printStructWithEnum(a: StructWithEnum): Json =
                    Json.obj(
                      "x" =>> a.x(),
                      "y" =>> Color.name(a.y()),
                      "z" =>> a.z()
                    )

                  val obj = Enums.getRootAsEnums(bb)
                  Json.obj(
                    "x" =>> Color.name(obj.x),
                    "y" =>> inside(obj.y)(printStructWithEnum),
                    "xs" =>> (0 until obj.xsLength()).map(obj.xs).map(Color.name),
                    "ys" =>> (0 until obj.ysLength()).map(obj.ys).map(printStructWithEnum)
                  ).some

                case "TableWithUnion" =>
                  val obj = TableWithUnion.getRootAsTableWithUnion(bb)
                  Json.obj(
                    "uni" =>> readWeapon(obj)(_.uniType, _.uni),
                  ).some

                case "Vectors" =>
                  val obj = Vectors.getRootAsVectors(bb)
                  Json.obj(
                    "a" =>> (0 until obj.aLength()).map(obj.a),
                    "b" =>> (0 until obj.bLength()).map(obj.b),
                    "c" =>> (0 until obj.cLength()).map(obj.c),
                    "d" =>> (0 until obj.dLength()).map(obj.d),
                    "e" =>> (0 until obj.eLength()).map(obj.e),
                    "f" =>> (0 until obj.fLength()).map(obj.f),
                    "g" =>> (0 until obj.gLength()).map(obj.g),
                    "h" =>> (0 until obj.hLength()).map(obj.h),
                    "i" =>> (0 until obj.iLength()).map(obj.i),
                    "j" =>> (0 until obj.jLength()).map(obj.j),
                    "k" =>> (0 until obj.kLength()).map(obj.k),
                    "l" =>> (0 until obj.lLength()).map(obj.l),
                  ).some

                case "Structs" =>
                  val obj = Structs.getRootAsStructs(bb)
                  Json.obj(
                    "w" =>> inside(obj.w) { w =>
                      Json.obj(
                        "x" =>> w.x,
                        "y" =>> w.y
                      )
                    },
                    "x" =>> inside(obj.x) { x =>
                      Json.obj(
                        "x" =>> x.x,
                        "y" =>> x.y
                      )
                    },
                    "y" =>> inside(obj.y) { y =>
                      Json.obj(
                        "w" =>> y.w,
                        "x" =>> y.x,
                        "y" =>> y.y,
                        "z" =>> y.z,
                      )
                    },
                    "z" =>> inside(obj.z) { z =>
                      Json.obj(
                        "x" =>> inside(z.x) { x =>
                          Json.obj(
                            "x" =>> x.x,
                            "y" =>> x.y
                          )
                        },
                        "y" =>> inside(z.y) { y =>
                          Json.obj(
                            "w" =>> y.w,
                            "x" =>> y.x,
                            "y" =>> y.y,
                            "z" =>> y.z,
                          )
                        }
                      )
                    }
                  ).some
                case "VectorOfTables" =>
                  val obj = VectorOfTables.getRootAsVectorOfTables(bb)
                  Json.obj(
                    "xs" =>> (0 until obj.xsLength()).map(obj.xs).map { simple =>
                      Json.obj(
                        "n" =>> simple.n,
                        "s" =>> simple.s
                      )
                    }
                  ).some

                case "VectorOfStructs" =>
                  val obj = VectorOfStructs.getRootAsVectorOfStructs(bb)
                  Json.obj(
                    "xs" =>> (0 until obj.xsLength()).map(obj.xs).map { threeBytes =>
                      Json.obj(
                        "x" =>> threeBytes.x,
                        "y" =>> threeBytes.y,
                        "z" =>> threeBytes.z
                      )
                    }
                  ).some

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

                case "AlignT" =>
                  def printAlign1(a: Align1): Json =
                    Json.obj(
                      "x" =>> a.x()
                    )

                  def printAlign2(a: Align2): Json =
                    Json.obj(
                      "x" =>> inside(a.x)(printAlign1),
                      "y" =>> a.y(),
                      "z" =>> a.z()
                    )

                  val obj = AlignT.getRootAsAlignT(bb)
                  Json.obj(
                    "x" =>> inside(obj.x)(printAlign1),
                    "y" =>> inside(obj.y)(printAlign2),
                    "xs" =>> (0 until obj.xsLength()).map(obj.xs).map(printAlign1),
                    "ys" =>> (0 until obj.ysLength()).map(obj.ys).map(printAlign2)
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
}


