package edu.gemini.spModel.pio.codec

import java.util.UUID

import scalaz._, Scalaz._

import edu.gemini.spModel.pio._
import edu.gemini.spModel.pio.xml.{ PioXmlUtil, PioXmlFactory }

trait ParamCodec[A] { outer =>
  def encode(key: String, a: A): Param
  def decode(p: Param): PioError \/ A
  def xmap[B](f: A => B, g: B => A): ParamCodec[B] =
    new ParamCodec[B] {
      def encode(key: String, a: B): Param = outer.encode(key, g(a))
      def decode(p: Param): PioError \/ B = outer.decode(p).map(f)
    }
}

object ParamCodec {
  
  private val pf = new PioXmlFactory

  def apply[A](implicit ev: ParamCodec[A]) = ev

  implicit val StringParamCodec: ParamCodec[String] =
    new ParamCodec[String] {
      def encode(key: String, a: String): Param = pf.createParam(key) <| (_.setValue(a))
      def decode(p: Param): PioError \/ String = Option(p.getValue) \/> NullValue(p.getName)
    }

  implicit val DoubleParamCodec: ParamCodec[Double] =
    new ParamCodec[Double] {
      def encode(key: String, a: Double): Param = StringParamCodec.encode(key, a.toString)
      def decode(p: Param): PioError \/ Double = StringParamCodec.decode(p) match {
        case \/-(s) => s.parseDouble.disjunction.leftMap(_ => ParseError(p.getName, s, "Double"))
        case -\/(e) => -\/(e)
      }          
    }

  implicit val IntParamCodec: ParamCodec[Int] =
    new ParamCodec[Int] {
      def encode(key: String, a: Int): Param = StringParamCodec.encode(key, a.toString)
      def decode(p: Param): PioError \/ Int = StringParamCodec.decode(p) match {
        case \/-(s) => s.parseInt.disjunction.leftMap(_ => ParseError(p.getName, s, "Int"))
        case -\/(e) => -\/(e)
      }          
    }

  implicit val LongParamCodec: ParamCodec[Long] =
    new ParamCodec[Long] {
      def encode(key: String, a: Long): Param = StringParamCodec.encode(key, a.toString)
      def decode(p: Param): PioError \/ Long = StringParamCodec.decode(p) match {
        case \/-(s) => s.parseLong.disjunction.leftMap(_ => ParseError(p.getName, s, "Long"))
        case -\/(e) => -\/(e)
      }          
    }

  implicit val UuidParamCodec: ParamCodec[UUID] =
    new ParamCodec[UUID] {
      def encode(key: String, a: UUID): Param = StringParamCodec.encode(key, a.toString)
      def decode(p: Param): \/[PioError, UUID] =
        StringParamCodec.decode(p).flatMap { s =>
          try UUID.fromString(s).right
          catch {
            case _: IllegalArgumentException => ParseError(p.getName, s, "UUID").left
          }
        }
    }

}


