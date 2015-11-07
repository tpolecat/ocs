package edu.gemini.spModel.target.env

import edu.gemini.shared.util.immutable.{Option => GOption}
import edu.gemini.spModel.pio.codec.{PioError, GeneralError, ParseError, ParamSetCodec}
import edu.gemini.spModel.pio.ParamSet
import edu.gemini.spModel.pio.xml.PioXmlFactory
import edu.gemini.spModel.target.{SPTargetPio, SPTarget}
import edu.gemini.spModel.rich.shared.immutable._

import scalaz._, Scalaz._

sealed trait BagsResult extends Product with Serializable with Cloneable { outer =>
  import BagsResult._

  def fold[A](nsp: => A, ntf: BagsChecksum => A, wt: (BagsChecksum, SPTarget) => A): A =
    this match {
      case NoSearchPerformed => nsp
      case NoTargetFound(vm) => ntf(vm)
      case WithTarget(vm, t) => wt(vm, t)
    }

  override def clone: BagsResult =
    fold(NoSearchPerformed, NoTargetFound, (m, t) => WithTarget(m, t.clone))

  def targetOption: Option[SPTarget] =
    fold(None, _ => None, (_, t) => Some(t))

  def versionMapOption: Option[BagsChecksum] =
    fold(None, Some(_), (m, _) => Some(m))

  // def rather than val so we don't have to serialize
  def asJava: JavaAPI =
    new JavaAPI

  class JavaAPI {

    def targetOption: GOption[SPTarget] =
      outer.targetOption.asGeminiOpt

    def versionMapOption: GOption[BagsChecksum] =
      outer.versionMapOption.asGeminiOpt

    def encode(key: String): ParamSet =
      BagsResult.BagsResultCodec.encode(key, outer)

  }

}

object BagsResult {

  case object NoSearchPerformed extends BagsResult
  case class NoTargetFound(versionMap: BagsChecksum) extends BagsResult
  case class WithTarget(versionMap: BagsChecksum, target: SPTarget) extends BagsResult

  implicit val SPTargetParamSetCodec: ParamSetCodec[SPTarget] =
    new ParamSetCodec[SPTarget] {
      def encode(key: String, a: SPTarget): ParamSet =
        SPTargetPio.getParamSet(a, new PioXmlFactory) <| (_.setName(key))
      def decode(ps: ParamSet): \/[PioError, SPTarget] =
        try {
          (new SPTarget <| (SPTargetPio.setParamSet(ps, _))).right
        } catch {
          case e: Exception => GeneralError("SPTarget").left
        }
    }

  val BagsResultDataCodec: ParamSetCodec[(Option[BagsChecksum], Option[SPTarget])] =
    ParamSetCodec.initial((none[BagsChecksum], none[SPTarget]))
      .withOptionalParamSet("checksum", Lens.firstLens[Option[BagsChecksum], Option[SPTarget]])
      .withOptionalParamSet("target", Lens.secondLens[Option[BagsChecksum], Option[SPTarget]])

  implicit val BagsResultCodec: ParamSetCodec[BagsResult] =
    BagsResultDataCodec.xmapF {
      case (None, None)       => NoSearchPerformed.right
      case (Some(v), None)    => NoTargetFound(v).right
      case (Some(v), Some(t)) => WithTarget(v, t).right
      case (None, Some(_))    => GeneralError("BagsResult").left
    } {
      case NoSearchPerformed  => (None, None)
      case NoTargetFound(v)   => (Some(v), None)
      case WithTarget(v, t)   => (Some(v), Some(t))
    }

  lazy val asJava: JavaCompanionAPI =
    new JavaCompanionAPI

  class JavaCompanionAPI {

    def decode(ps: ParamSet): BagsResult =
      BagsResultCodec.decode(ps).fold(a => sys.error(a.toString), identity)

    def noSearchPerformed: BagsResult =
      NoSearchPerformed

    def noTargetFound(versionMap: BagsChecksum): BagsResult =
      NoTargetFound(versionMap)

    def withTarget(versionMap: BagsChecksum, target: SPTarget): BagsResult =
      WithTarget(versionMap, target)

  }

}
