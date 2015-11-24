package jsky.app.ot.tpe

import edu.gemini.ags.gems.GemsGuideStars
import edu.gemini.pot.sp.SPNodeKey
import edu.gemini.shared.util.immutable.{ Option => GOption }
import edu.gemini.shared.util.immutable.ScalaConverters._
import edu.gemini.spModel.obscomp.SPInstObsComp
import edu.gemini.spModel.target.SPTarget
import edu.gemini.spModel.target.env.{BagsChecksum, BagsResult, GuideProbeTargets, GuideGroup, GuideEnvironment, TargetEnvironment}
import jsky.app.ot.ags.BagsManager

class GemsGuideStarWorkerHelper {

  def applyResults(ctx: TpeContext,  gemsGuideStarsOpt: GOption[GemsGuideStars], isBags: Boolean): Unit =
    applyResults(ctx, gemsGuideStarsOpt.asScalaOpt, isBags);

  def applyResults(ctx: TpeContext,  gemsGuideStarsOpt: Option[GemsGuideStars], isBags: Boolean): Unit = {

    // Update the position angle, if needed, and if so return the inst node key
    val instNodeKey: Option[SPNodeKey] = gemsGuideStarsOpt.map(_.pa.toDegrees).flatMap { newPosAngle =>
      val inst: Option[SPInstObsComp] = ctx.instrument.dataObject
      inst.filter(_.getPosAngleDegrees != newPosAngle).flatMap { inst =>
        inst.setPosAngleDegrees(newPosAngle)
        ctx.instrument.commit();
        ctx.instrument.shell.map(_.getNodeKey)
      }
    }

    // The target node key, if any
    val tgtNodeKey: Option[SPNodeKey] =
      ctx.targets.shell.map(_.getNodeKey)

    // Function to construct a BAGS result
    val f: SPTarget => BagsResult = {
      val crc0 = ctx.obsShell.map(BagsChecksum.initialHash).getOrElse(BagsChecksum.empty)
      val crc1 = (instNodeKey.toSet ++ tgtNodeKey.toSet).foldLeft(crc0)(_ + _)
      BagsResult.WithTarget(crc1, _)
    }

    // Update the TargetObsComp with the GEMS result
    ctx.targets.dataObject.foreach { targetObsComp =>

      // Our original new target environments
      val oldTargetEnv: TargetEnvironment = targetObsComp.getTargetEnvironment
      val newTargetEnv: TargetEnvironment = gemsGuideStarsOpt.map(_.guideGroup) match {
        case Some(gemsGuideGroup) if isBags =>

          // Update GuideProbeTargets to reflect the fact that the result came from BAGS
          val updatedGuideProbeTargets: List[GuideProbeTargets] =
            gemsGuideGroup.getAll.asScalaList.map { gpts =>
              gpts.getPrimary.asScalaOpt match {
                case Some(t) => gpts.removeTarget(t).withBagsResult(f(t))
                case None    => gpts
              }
            }

          // Make the new bags group primary if there were no guide groups before, or if the old
          // primary group was a BAGS group.
          val makeBagsGroupPrimary = {
            val oldGuideEnv = oldTargetEnv.getGuideEnvironment
            oldGuideEnv.getOptions.isEmpty || oldGuideEnv.getPrimary.asScalaOpt.exists(isBagsGroup)
          }

          // The final target env
          val clearedTargetEnv  = BagsManager.clearBagsTargets(oldTargetEnv)
          val updatedGuideGroup = gemsGuideGroup.putAll(updatedGuideProbeTargets.asImList)
          if (makeBagsGroupPrimary) {
            clearedTargetEnv.setPrimaryGuideGroup(updatedGuideGroup)
          } else {
            val newGuideEnv = clearedTargetEnv.getGuideEnvironment().setOptions(clearedTargetEnv.getGroups().cons(updatedGuideGroup))
            clearedTargetEnv.setGuideEnvironment(newGuideEnv)
          }

        case Some(gemsGuideGroup) => oldTargetEnv.setPrimaryGuideGroup(gemsGuideGroup)
        case None                 => oldTargetEnv

      }

      // If BAGS is running, only change if the target environments differ.
      if (!isBags || !BagsManager.bagsTargetsMatch(oldTargetEnv, newTargetEnv)) {
        targetObsComp.setTargetEnvironment(newTargetEnv)
        ctx.targets.commit()
      }

    }
  }


  /**
   * A GuideGroup is considered to be a "BAGS group" if it contains at least one GuideProbeTargets
   * where the primary target was assigned by BAGS. It's unclear how much sense this makes.
   */
  def isBagsGroup(gg: GuideGroup): Boolean =
    gg.getAll.asScalaList.exists(_.primaryIsBagsTarget)

}
