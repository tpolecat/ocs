package edu.gemini.dataman.gsa.query

import edu.gemini.dataman.gsa.query.JsonCodecs._
import edu.gemini.spModel.dataset.{DatasetLabel, DatasetQaState}

import argonaut._
import Argonaut._

import java.time.Instant

/** Representation of a GSA file record, with only the parts we care about for
  * display in the OT and processing in the Data Manager.
  *
  * The `time` parameter is the GSA ingestion instant which, lacking a more
  * explicit alternative, is used as a version number. */
case class GsaFile(label: DatasetLabel, qa: DatasetQaState, time: Instant, filename: String)

object GsaFile {
  implicit def CodecJsonGsaFile: CodecJson[GsaFile] =
    casecodec4(GsaFile.apply, GsaFile.unapply)("data_label", "qa_state", "entrytime", "filename")
}