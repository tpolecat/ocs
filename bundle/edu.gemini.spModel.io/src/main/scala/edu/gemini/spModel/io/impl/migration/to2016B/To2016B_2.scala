package edu.gemini.spModel.io.impl.migration.to2016B

import edu.gemini.pot.sp.SPComponentType
import edu.gemini.spModel.core.{Site, Ephemeris, Declination, RightAscension, Coordinates}
import edu.gemini.spModel.io.impl.migration.Migration
import edu.gemini.spModel.pio.codec.ParamCodec
import edu.gemini.spModel.pio.xml.PioXmlFactory
import edu.gemini.spModel.pio.{Document, ParamSet, Pio, Version}
import edu.gemini.spModel.io.impl.migration.PioSyntax._

import scalaz._, Scalaz._

/** Conversion from the initial 2016B release ephemeris style to the updated
  * compressed data style.
  */
object To2016B_2 extends Migration {

  val version = Version.`match`("2016B-2")

  val EphemerisElement = "ephemeris-element"
  val Factory          = new PioXmlFactory

  val conversions: List[Document => Unit] = List(
    updateEphemeris
  )

  private def updateEphemeris(d: Document): Unit = {
    def toEphemerisElement(ps: ParamSet): (Long, Coordinates) = {
      val time = Pio.getLongValue(ps,   "time",            0l)
      val raD  = Pio.getDoubleValue(ps, "coordinates/ra",  0.0)
      val decD = Pio.getDoubleValue(ps, "coordinates/dec", 0.0)
      val ra   = RightAscension.fromDegrees(raD)
      val dec  = Declination.fromDegrees(decD).get
      (time, Coordinates(ra, dec))
    }

    d.findContainers(SPComponentType.TELESCOPE_TARGETENV).foreach { c =>
      c.allParamSets.filter(_.getName == "ephemeris").foreach { eph =>
        val site      = Site.parse(Pio.getValue(eph, "site", "GN"))
        val elements  = eph.paramSets.filter(_.getName == EphemerisElement).map(toEphemerisElement)

        // If we started with a pre-2016B XML document then To2016B will have
        // already updated the XML and elements will be empty.  If that is the
        // case, there is nothing to do.  If non-empty, we have initial 2016B
        // expanded ephemeris elements that need to be converted to compressed
        // data.
        if (elements.nonEmpty) {
          val ephemeris = Ephemeris(site, ==>>.fromList(elements))
          eph.removeChildren(EphemerisElement)
          eph.addParam(ParamCodec.deflatedParamCodec.encode("data", ephemeris.compressedData))
        }
      }
    }
  }
}