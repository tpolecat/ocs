package edu.gemini.spModel.target.env

import edu.gemini.shared.util.immutable.{Option => GOption}
import edu.gemini.spModel.pio.codec.ParamSetCodec
import edu.gemini.spModel.pio.ParamSet
import edu.gemini.spModel.target.SPTarget
import edu.gemini.spModel.rich.shared.immutable._

sealed abstract class BagsResult extends Cloneable { outer =>
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

  lazy val asJava: JavaAPI =
    new JavaAPI

  class JavaAPI {

    def targetOption: GOption[SPTarget] =
      outer.targetOption.asGeminiOpt

    def versionMapOption: GOption[BagsChecksum] =
      outer.versionMapOption.asGeminiOpt

    def encode(key: String): ParamSet =
      ???

  }

}

object BagsResult {

  case object NoSearchPerformed extends BagsResult
  case class NoTargetFound(versionMap: BagsChecksum) extends BagsResult
  case class WithTarget(versionMap: BagsChecksum, target: SPTarget) extends BagsResult

  implicit val CodecBagsResult: ParamSetCodec[BagsResult] =
    ???

  lazy val asJava: JavaCompanionAPI =
    new JavaCompanionAPI

  class JavaCompanionAPI {

    def decode(ps: ParamSet): BagsResult =
      CodecBagsResult.decode(ps).fold(a => sys.error(a.toString), identity)

    def noSearchPerformed: BagsResult =
      NoSearchPerformed

    def noTargetFound(versionMap: BagsChecksum): BagsResult =
      NoTargetFound(versionMap)

    def withTarget(versionMap: BagsChecksum, target: SPTarget): BagsResult =
      WithTarget(versionMap, target)

  }

}
