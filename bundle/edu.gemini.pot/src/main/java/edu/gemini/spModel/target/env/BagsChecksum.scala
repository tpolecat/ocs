package edu.gemini.spModel.target.env

import java.util.UUID

import edu.gemini.pot.sp.{ISPObservation, SPNodeKey}
import edu.gemini.pot.sp.version.{ VersionMap, vmChecksum, subVersionMap }
import edu.gemini.shared.util.VersionVector
import edu.gemini.shared.util.IntegerIsIntegral
import edu.gemini.spModel.pio.codec.{ParamCodec, ParamSetCodec}

import scalaz._

case class BagsChecksum(modifiedKeys: Set[SPNodeKey], hash: Long) {

  def isConsistentWithObservation(obs: ISPObservation): Boolean =
    isConsistentWithVersionMap(subVersionMap(obs))

  def isConsistentWithVersionMap(vm: VersionMap): Boolean =
    vmChecksum(modifiedKeys.foldLeft(vm) { (m, k) =>
      m.get(k) match {
        case Some(v) => m + (k -> VersionVector(v.clocks.mapValues(n => (n - 1): Integer).toMap))
        case None => m
      }
    }) == hash

  def +(k: SPNodeKey): BagsChecksum =
    copy(modifiedKeys = modifiedKeys + k)

}

object BagsChecksum {

  val empty: BagsChecksum =
    BagsChecksum(Set.empty, 0L)

  def initialHash(obs: ISPObservation): BagsChecksum =
    apply(Set.empty, vmChecksum(subVersionMap(obs)))

  val hash:            BagsChecksum @> Long            = Lens.lensu((a, b) => a.copy(hash = b), _.hash)
  val modifiedKeySet:  BagsChecksum @> Set[SPNodeKey]  = Lens.lensu((a, b) => a.copy(modifiedKeys = b), _.modifiedKeys)
  val modifiedKeyList: BagsChecksum @> List[SPNodeKey] = modifiedKeySet.xmapB(_.toList)(_.toSet)

  implicit val SPNodeKeyParamCodec: ParamCodec[SPNodeKey] =
    ParamCodec[UUID].xmap(new SPNodeKey(_), _.uuid)

  implicit val BagsChecksumParamSetCodec: ParamSetCodec[BagsChecksum] =
    ParamSetCodec.initial(empty)
      .withParam("hash", hash)
      .withManyParams("modified-keys", modifiedKeyList)

}
