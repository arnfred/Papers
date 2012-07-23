package paper
import java.io.File

// This class transforms a File object into another File object, according to the extention of the file
abstract class FileFormat {  
	def parseToTXT: File = this match{
    case TXTFormat(file) => file
    case PDFFormat(file) => {
      if(Paths.isWindows) {
	    val command = "\"" + Paths.toolsDir + Paths.windowsDir + Paths.windowsTool + "32" + Paths.windowsExt + "\" -enc UTF-8 \"" + file.getAbsoluteFile() + "\""
	    val process: Process = sys.runtime.exec(command)    
	    
	    // Waiting until the end of the command execution
	    if(process.waitFor() != 0) sys.error("Can't parse pdf file. Program will exit")
	    
	    val name = ("""\..+$""".r.replaceAllIn(file.getName(), ""))
	    new File(file.getParent() + "\\" + name + Paths.TXTExt)
      }
      else file
	 }
  }
}

case class TXTFormat (file: File) extends FileFormat

case class PDFFormat (file: File) extends FileFormat

object Paths {
  val TXTExt = ".txt"
  val toolsDir = "tools\\"  
  val linuxDir = "linux\\"
  val windowsDir = "windows\\"
  val windowsExt = ".exe"
  val windowsTool = "pdftotext"
  private val os = sys.props.get("os.name")
  
  // The apps is on Windows
  def isWindows: Boolean = os match {
    case Some(s) => """.*Windows.*""".r.findFirstIn(s.toString()).isDefined
    case None => false
  }
  
  // The apps is on Unix (Linux) (probably NOT CORRECT!!!)
  def isUnix: Boolean = os match {
    case Some(s) => """.*Unix.*""".r.findFirstIn(s.toString()).isDefined
    case None => false
  }
}


object FileFormatDispatcher {
	private def ext(file: String) = """\..+$""".r.findFirstIn(file).get
	private def name(file: String) = """\..+$""".r.replaceAllIn(file, "")
	
	// Determines which FileFormat should be used according to the extension of the file
	def getFileFormat(file: File): FileFormat = {
	  
	  ext(file.getName()) match {
	    case ".txt" => new TXTFormat(file)
	    case ".pdf" => new PDFFormat(file)
	  }
	}
	
	// Performs final procedures before the end of the file processing (mostly deleting intermediate files)
	def releaseFile(file: File) = {
	  ext(file.getName()) match {
	    case ".txt" =>
	    case ".pdf" => {
	      if(Paths.isWindows){
	         val process: Process = sys.runtime.exec("tools\\cmd\\del.bat \"" + file.getParent() + "\\" + name(file.getName()) + Paths.TXTExt)
	         // Waiting until the end of the command execution
	         if(process.waitFor() != 0) sys.error("Can't release pdf file. Program will exit")
	      }
	    }
	  }
	}
}