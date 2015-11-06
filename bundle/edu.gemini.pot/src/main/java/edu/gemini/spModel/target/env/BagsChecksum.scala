package edu.gemini.spModel.target.env

import edu.gemini.pot.sp.{ISPObservation, SPNodeKey}
import edu.gemini.pot.sp.version.{ VersionMap, vmChecksum, subVersionMap }
import edu.gemini.shared.util.VersionVector
import edu.gemini.shared.util.IntegerIsIntegral

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

}
