package edu.gemini.spModel.inst

import java.awt.Shape
import java.awt.geom.{Point2D, AffineTransform}

import edu.gemini.shared.util.immutable.{DefaultImList, ImList}
import edu.gemini.spModel.core.Angle
import edu.gemini.spModel.obs.context.ObsContext
import edu.gemini.spModel.rich.shared.immutable._

import scala.collection.JavaConverters._

/**
 * Functions to manipulate the geometry for a telescope feature, e.g. a science area or a
 * guide probe arm.
 */
object FeatureGeometry {
  /**
   * Convenience method to execute a transformation on a point and return the result.
   * @param p     the point to transform
   * @param trans the transformation to execute
   * @return      the transformed point
   */
  def transformPoint(p: Point2D, trans: AffineTransform): Point2D = {
    val pTrans = new Point2D.Double()
    trans.transform(p, pTrans)
    pTrans
  }

  /**
   * Given a list of shapes representing a science area, transform them as needed for the specified context.
   * @param shapes the list of shapes to transform
   * @param ctx    the context under which to transform them
   * @return       the transformed shapes
   */
  def transformScienceAreaForContext(shapes: List[Shape], ctx: ObsContext): List[Shape] = {
    val posAngle = ctx.getPositionAngle.toRadians.getMagnitude
    val basePoint = {
      val coords = ctx.getBaseCoordinates
      new Point2D.Double(coords.getDec.getMagnitude, coords.getRa.getMagnitude)
    }
    val trans = AffineTransform.getRotateInstance(-posAngle, basePoint.getX, basePoint.getY)
    trans.translate(basePoint.getX, basePoint.getY)
    shapes.map{ trans.createTransformedShape }
  }

  def transformScienceAreaForContextAsJava(shapes: List[Shape], ctx: ObsContext): ImList[Shape] =
    DefaultImList.create(transformScienceAreaForContext(shapes, ctx).asJava)
  def transformScienceAreaForContextAsJava(shapes: ImList[Shape], ctx: ObsContext): ImList[Shape] =
    transformScienceAreaForContextAsJava(shapes.asScalaList, ctx)

  /**
   * Given a single shape representing part or all of a science area, transform it as needed for the context.
   * @param shape the shape to transform
   * @param ctx   the context under which to transform it
   * @return      the transformed shape
   */
  def transformScienceAreaForContext(shape: Shape, ctx: ObsContext): Shape =
    transformScienceAreaForContext(List(shape), ctx).head

  /**
   * Given a list of shapes representing a science area, transform it to screen coordinates.
   * @param shapes          the list of shapes to transform
   * @param pixelsPerArcsec the pixel density
   * @return                the shapes scaled for the display
   */
  def transformScienceAreaForScreen(shapes: List[Shape], pixelsPerArcsec: Double): List[Shape] = {
    val scaleTrans = AffineTransform.getScaleInstance(pixelsPerArcsec, pixelsPerArcsec)
    shapes.map {scaleTrans.createTransformedShape }
  }

  /**
   * Given a shape representing part or all of a science area, transform it to screen coordinates.
   * @param shape          the shape to transform
   * @param pixelsPerArcsec the pixel density
   * @return                the shape scaled for the display
   */
  def transformScienceAreaForScreen(shape: Shape, pixelsPerArcsec: Double): Shape =
    transformScienceAreaForScreen(List(shape), pixelsPerArcsec).head

  /**
   * Given a list of shapes representing a guide probe arm, an angle for the probe arm, and the position of the guide
   * star, execute transformations on the shapes to get the adjusted probe arm.
   * @param shapes    the list of shapes to transform
   * @param armAngle  the angle at which the probe arm will be situated
   * @param guideStar the position of the guide star in arcseconds
   * @return          the transformed shapes
   */
  def transformProbeArmForContext(shapes: List[Shape], armAngle: Double, guideStar: Point2D): List[Shape] = {
    // For the guide star, we want to use the point closest to zero in terms of arcsec as a normalization.
    val armTrans = AffineTransform.getRotateInstance(armAngle, guideStar.getX, guideStar.getY)
    armTrans.concatenate(AffineTransform.getTranslateInstance(guideStar.getX, guideStar.getY))
    shapes.map { armTrans.createTransformedShape }
  }

  /**
   * Given a shape representing a guide probe arm or segment, an angle for the probe arm, and the position of the guide
   * star, execute transformations on the shape to get the adjusted probe arm.
   * @param shape     the shape to transform
   * @param armAngle  the angle at which the probe arm will be situated
   * @param guideStar the position of the guide star in arcsec
   * @return          the transformed shapes
   */
  def transformProbeArmForContext(shape: Shape, armAngle: Double, guideStar: Point2D): Shape =
    transformProbeArmForContext(List(shape), armAngle, guideStar).head

  /**
   * Given a list of geometry shapes, transform them as needed for display on the screen.
   * @param shapes          the list of shapes to transform
   * @param pixelsPerArcsec the pixel density per arcsec on the current display
   * @param xFlipArm        true if the arm should be flipped in the x-axis, e.g. if the ISS port is side looking
   * @param flipRA          a scaling in the y-axis: should be either 1.0 or -1.0, and not sure if this is ever -1.0
   * @param screenPos       the final position on the screen where the adjusted guide probe arm should be placed
   * @return                the transformed shape
   */
  def transformProbeArmForScreen(shapes: List[Shape], pixelsPerArcsec: Double, xFlipArm: Boolean, flipRA: Double, screenPos: Point2D): List[Shape] = {
    //val xFlipFactor = if (xFlipArm) -1.0 else 1.0
    val trans = AffineTransform.getTranslateInstance(screenPos.getX, screenPos.getY)
    trans.scale(pixelsPerArcsec, pixelsPerArcsec)
    //trans.scale(flipRA, xFlipFactor)
    shapes.map { trans.createTransformedShape }
  }

  /**
   * Given a shape, transform it as needed for display on the screen.
   * @param shape           the shape to transform
   * @param pixelsPerArcsec the pixel density per arcsec on the current display
   * @param xFlipArm        true if the arm should be flipped in the x-axis, e.g. if the ISS port is side looking
   * @param flipRA          a scaling in the y-axis: should be either 1.0 or -1.0, and not sure if this is ever -1.0
   * @param screenPos       the final position on the screen where the adjusted guide probe arm should be placed
   * @return                the transformed shape
   */
  def transformProbeArmForScreen(shape: Shape, pixelsPerArcsec: Double, xFlipArm: Boolean, flipRA: Double, screenPos: Point2D): Shape =
    transformProbeArmForScreen(List(shape), pixelsPerArcsec, xFlipArm, flipRA, screenPos).head
}
