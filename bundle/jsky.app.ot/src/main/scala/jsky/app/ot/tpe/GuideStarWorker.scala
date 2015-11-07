package jsky.app.ot.tpe

import edu.gemini.ags.api.AgsStrategy
import edu.gemini.pot.sp.SPNodeKey
import edu.gemini.spModel.guide.GuideProbe
import edu.gemini.spModel.target.SPTarget
import edu.gemini.spModel.target.env.{BagsChecksum, BagsResult}
import jsky.app.ot.ags.BagsManager
import jsky.app.ot.gemini.altair.Altair_WFS_Feature
import jsky.app.ot.gemini.inst.OIWFS_Feature
import jsky.app.ot.gemini.tpe.TpePWFSFeature

// Perform the lookup of non-GeMS guide stars and apply the results to the TPE.
object GuideStarWorker {
  def applyResults(ctx: TpeContext, selOpt: Option[AgsStrategy.Selection]): Unit = {
    applySelection(ctx, selOpt)
    showTpeFeatures(selOpt)
  }

  private def showTpeFeatures(selOpt: Option[AgsStrategy.Selection]): Unit =
    selOpt.foreach { sel =>
      Option(TpeManager.get()).filter(_.isVisible).foreach { tpe =>
        sel.assignments.foreach { ass =>
          val clazz = ass.guideProbe.getType match {
            case GuideProbe.Type.AOWFS => classOf[Altair_WFS_Feature]
            case GuideProbe.Type.OIWFS => classOf[OIWFS_Feature]
            case GuideProbe.Type.PWFS => classOf[TpePWFSFeature]
          }
          Option(tpe.getFeature(clazz)).foreach {
            tpe.selectFeature
          }
        }
      }
    }

  private def applySelection(ctx: TpeContext, selOpt: Option[AgsStrategy.Selection]): Unit = {

    // Update the position angle, if necessary, returning the key of the updated node, if any.
    val instNodeKey: Option[SPNodeKey] =
      selOpt.flatMap { sel =>
        ctx.instrument.dataObject.foreach { inst =>
          val deg = sel.posAngle.toDegrees
          val old = inst.getPosAngleDegrees
          if (deg != old) {
            inst.setPosAngleDegrees(deg)
            ctx.instrument.commit()
          }
        }
        ctx.instrument.shell.map(_.getNodeKey)
      }

    // If we need to update the target env, this is its key
    val tgtNodeKey: Option[SPNodeKey] =
      ctx.targets.shell.map(_.getNodeKey)

    // Find out which guide probes previously had assignments, but no longer do.
    val oldEnv = ctx.targets.envOrDefault

    // Clear out the old BAGS targets
    val clearedEnv = BagsManager.clearBagsTargets(oldEnv)

    // Function to construct a BAGS result
    val f: SPTarget => BagsResult = {
      val crc0 = ctx.obsShell.map(BagsChecksum.initialHash).getOrElse(BagsChecksum.empty)
      val crc1 = (instNodeKey.toSet ++ tgtNodeKey.toSet).foldLeft(crc0)(_ + _)
      BagsResult.WithTarget(crc1, _)
    }

    // Construct a new environment with the updated target information.
    val newEnv = selOpt.fold(clearedEnv)(_.applyTo(clearedEnv, f))

    // Update the TargetEnvironment if it is different.
    if (!BagsManager.bagsTargetsMatch(oldEnv, newEnv)) {
      ctx.targets.dataObject.foreach { targetComp =>
        targetComp.setTargetEnvironment(newEnv)
        ctx.targets.commit()
      }
    }
  }
}
