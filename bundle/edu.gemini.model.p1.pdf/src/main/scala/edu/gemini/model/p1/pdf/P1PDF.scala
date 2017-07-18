package edu.gemini.model.p1.pdf

import scala.util.Try
import xml.{Node, XML}
import java.io._
import javax.xml.transform.URIResolver
import javax.xml.transform.stream.StreamSource
import edu.gemini.util.pdf.PDF
import io.Source

/**
 * Creator object for pdf creation for Phase1 documents.
 */
object P1PDF {

  sealed trait InvestigatorsListOption {
    def param: String
  }

  object InvestigatorsListOption {
    val InvestigatorsListParam = "investigatorsList"
    case object DefaultList extends InvestigatorsListOption {
      val param = "default"
    }
    case object AtTheEndList extends InvestigatorsListOption {
      val param = "atTheEnd"
    }
    case object NoList extends InvestigatorsListOption {
      val param = "no"
    }
  }

  sealed case class Template(name: String, location: String, pageSize: PDF.PageSize, investigatorsList: InvestigatorsListOption, params: Map[String, String]) {
    def value(): String = name
    val parameters: Map[String, String] = params + (InvestigatorsListOption.InvestigatorsListParam -> investigatorsList.param)
  }

  object GeminiDefault extends Template(
    "Gemini Default", "templates/xsl-default.xml", PDF.Letter, InvestigatorsListOption.DefaultList,
    Map("partner"->"gs", "pageLayout" -> "default-us-letter", "title" -> "GEMINI OBSERVATORY"))

  object GeminiDefaultNoInvestigatorsList extends Template(
    "Gemini No CoIs", "templates/xsl-default.xml", PDF.Letter, InvestigatorsListOption.NoList,
    Map("partner"->"gs", "pageLayout" -> "default-us-letter", "title" -> "GEMINI OBSERVATORY"))

  object GeminiDefaultListAtTheEnd extends Template(
    "Gemini CoIs at End", "templates/xsl-default.xml", PDF.Letter, InvestigatorsListOption.AtTheEndList,
    Map("partner"->"gs", "pageLayout" -> "default-us-letter", "title" -> "GEMINI OBSERVATORY"))

  object AU extends Template(
    "Australian NGO", "templates/xsl-default.xml", PDF.Letter, InvestigatorsListOption.AtTheEndList,
    Map("partner"->"au", "pageLayout" -> "default-us-letter", "title" -> "GEMINI OBSERVATORY"))

  object CL extends Template(
    "Chilean NGO", "templates/xsl-default.xml",    PDF.Letter, InvestigatorsListOption.AtTheEndList,
    Map("partner"->"cl", "pageLayout" -> "default-us-letter", "title" -> "PROPUESTA CONICYT-Gemini"))

  object NOAO extends Template(
    "NOAO",   "templates/xsl-NOAO.xml",            PDF.Letter, InvestigatorsListOption.AtTheEndList,
    Map("partner"->"us", "pageLayout" -> "default-us-letter"))

  object NOAONoInvestigatorsList extends Template(
    "NOAO No CoIs",   "templates/xsl-NOAO.xml",            PDF.Letter, InvestigatorsListOption.NoList,
    Map("partner"->"us", "pageLayout" -> "default-us-letter"))

  /** Gets a list with all templates that are currently available. */
  def templates = List(GeminiDefault, GeminiDefaultNoInvestigatorsList, GeminiDefaultListAtTheEnd, AU, CL, NOAO, NOAONoInvestigatorsList)

  def templatesMap = Map(
    "ar"     -> GeminiDefault,
    "au"     -> AU,
    "br"     -> GeminiDefault,
    "ca"     -> GeminiDefaultListAtTheEnd,
    "cl"     -> CL,
    "kr"     -> GeminiDefault,
    "uh"     -> GeminiDefault,
    "gs"     -> GeminiDefault,
    "gsiend" -> GeminiDefaultListAtTheEnd,
    "gsnoi"  -> GeminiDefaultNoInvestigatorsList,
    "us"     -> NOAO,
    "usnoi"  -> NOAONoInvestigatorsList)

  /**
   * Creates a pdf from a given xml file and template and writes the resulting pdf file to the output folder.
   * This method also merges the attached pdf file to the end of the resulting pdf.
   */
  def createFromFile (xmlFile: File, template: Template, pdfFile: File) {
    Try(createFromNode(XML.loadFile(xmlFile), template, pdfFile, Option(xmlFile.getParentFile))).getOrElse(createFromNode(XML.loadString(Source.fromFile(xmlFile, "latin1").mkString), template, pdfFile, Option(xmlFile.getParentFile)))
  }

  /**
   * Creates a pdf from a given xml element and template and writes the resulting pdf file to the output folder.
   * This method also merges the attached pdf file to the end of the resulting pdf.
   */
  def createFromNode (xml: Node, template: Template, pdfFile: File, workingDir:Option[File] = None) {
    val attachment = {
      val f = new File((xml \ "meta" \ "attachment").text)
      if (f.isAbsolute) f else workingDir.map(new File(_, f.getPath)).getOrElse(f)
    }
    createFromNode(xml, attachment, template, pdfFile, workingDir)
  }


  def createFromNode(xml: Node, attachment: File, template: Template, out: File, workingDir: Option[File]) {
    val pdf = new PDF(Some(P1PdfUriResolver))

    def using[A, B](resource: => A)(cleanup: A => Unit)(code: A => B): Option[B] = {
      try {
        val r = resource
        try { Some(code(r)) }
        finally {
            cleanup(r)
        }
      } catch {
        case e: Exception => None
      }
    }

    def runTransformation(destination: File, template: Template): Option[File] = {
      using(getClass.getResourceAsStream(template.location))(_.close) { xslStream =>
        val xmlSource = new StreamSource(new StringReader(xml.toString()))
        val xslSource = new StreamSource(xslStream)
        pdf.transformXslFo(xmlSource, xslSource, destination, template.parameters)
        destination
      }
    }

    val parentFilePath = Option(out.getParentFile).getOrElse("")
    val intermediateOutputFile = new File(parentFilePath + File.separator + "_" + out.getName)
    val intermediateILFile = new File(parentFilePath + File.separator + "_" + out.getName + "_il")

    val maybeAttachment = if (attachment.isFile) Some(attachment) else None
    val filesToMerge = template.investigatorsList match {
      case InvestigatorsListOption.AtTheEndList =>
        val main = runTransformation(intermediateOutputFile, template.copy(investigatorsList = InvestigatorsListOption.NoList))
        val investigatorsList = runTransformation(intermediateILFile, template)
        List(main, maybeAttachment, investigatorsList).flatten
      case _                                   =>
        val main = runTransformation(intermediateOutputFile, template)
        List(main, maybeAttachment).flatten
    }
    pdf.merge(filesToMerge, out, template.pageSize)
    intermediateOutputFile.delete()
    intermediateILFile.delete()
  }


  /**
   * This is a very crappy single purpose URI resolver. It will allow the XML libs used in the PDF bundle
   * to resolve resources imported with <xsl:import/>. This is necessary since the PDF bundle does not have
   * access to the resources in this bundle.
   */
  case object P1PdfUriResolver extends URIResolver {
    override def resolve(href: String,  base: String) = {
      val r = Option(getClass.getResourceAsStream(href))
      // try to resolve as a normal resource (assuming that caller closes stream?)
      r.map(new StreamSource(_)).getOrElse {
        if (new File(href).exists) {
          new StreamSource(new File(href)) // try to resolve as a file
        } else {
          new StreamSource(href)           // try to resolve as a URL
        }
      }
    }
  }

  def main(args:Array[String]) {
    val home = System.getProperty("user.home")
    val in = new File(s"$home/pitsource.xml")
    val out = new File(s"$home/pittarget.pdf")
    createFromFile(in, NOAONoInvestigatorsList, out)

    val ok = Runtime.getRuntime.exec(Array("open", out.getAbsolutePath)).waitFor
    println("Exec returned " + ok)
    System.exit(0)
  }
}
