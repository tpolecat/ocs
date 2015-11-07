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
    val instNodeKey: Option[SPNodeKey] = gemsGuideStarsOpt.flatMap { gemsGuideStars =>
      val inst: Option[SPInstObsComp] = ctx.instrument.dataObject
      val newPosAngle: Double = gemsGuideStars.pa.toDegrees
      inst.filter(_.getPosAngleDegrees != newPosAngle).flatMap { inst =>
        inst.setPosAngleDegrees(newPosAngle)
        ctx.instrument.commit();
        ctx.instrument.shell.map(_.getNodeKey)
      }
    };

    // The target node key, if any
    val tgtNodeKey: Option[SPNodeKey] =
      ctx.targets.shell.map(_.getNodeKey)

    // Function to construct a BAGS result
    val f: SPTarget => BagsResult = {
      val crc0 = ctx.obsShell.map(BagsChecksum.initialHash).getOrElse(BagsChecksum.empty)
      val crc1 = (instNodeKey.toSet ++ tgtNodeKey.toSet).foldLeft(crc0)(_ + _)
      BagsResult.WithTarget(crc1, _)
    }

    ctx.targets.dataObject.foreach { targetObsComp =>

      val oldEnv: TargetEnvironment = targetObsComp.getTargetEnvironment

      // If this is called from BAGS, we need to find out if there was a previous BAGS guide group and whether
      // or not it was the primary group.
      val (makeBagsGroupPrimary, clearedEnv) =
        if (isBags) {
          val guideEnv: GuideEnvironment    = oldEnv.getGuideEnvironment
          val bagsGroup: Option[GuideGroup] = guideEnv.getOptions.asScalaList.find(_.getAll.asScalaList.exists(_.primaryIsBagsTarget))
          val b = guideEnv.getOptions.isEmpty || bagsGroup.exists(bg => oldEnv.getGuideEnvironment.getPrimary.asScalaOpt.exists(bg == _))
          val e = BagsManager.clearBagsTargets(oldEnv)
          (b, e)
      } else (false, oldEnv)

      // If this is BAGS running, denote the primary selected targets as the BAGS targets.
      // This is a horrible way to do things, but we don't want to mess with the actual GeMS lookup code so we
      // transform the guide group as necessary for BAGS.
      val finalEnv: TargetEnvironment = gemsGuideStarsOpt.map { gemsGuideStars =>
        // Determine / adapt the new guide group representing the GeMS selection.
        if (isBags) {
          val gptList: List[GuideProbeTargets] = gemsGuideStars.guideGroup.getAll.asScalaList.map(gpt =>
            gpt.getPrimary.asScalaOpt.map(primary => gpt.removeTarget(primary).withBagsResult(f(primary))).getOrElse(gpt)
          )
          val group = gemsGuideStars.guideGroup.putAll(gptList.asImList)
          if (makeBagsGroupPrimary)
            clearedEnv.setPrimaryGuideGroup(group)
          else
              clearedEnv.setGuideEnvironment(clearedEnv.getGuideEnvironment().setOptions(clearedEnv.getGroups().cons(group)));
        } else {
          val group = gemsGuideStars.guideGroup
          clearedEnv.setPrimaryGuideGroup(group)
        }
      }.getOrElse(clearedEnv)

      // If BAGS is running, only change if the target environments differ.
      if (!isBags || !BagsManager.bagsTargetsMatch(oldEnv, finalEnv)) {
        targetObsComp.setTargetEnvironment(finalEnv)
        ctx.targets.commit()
      }

    }
  }
}
