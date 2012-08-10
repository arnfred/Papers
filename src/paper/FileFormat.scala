package paper
import java.io.File

// This class transforms a File object into another File object, according to the extension of the file
abstract class FileFormat {  
	def parseTo(format: String, params: String): File = this match{
    case TXTFormat(file) => file
    case PDFFormat(file) => {
      if(Paths.isWindows) {
	    val command = "\"" + Paths.toolsDir + Paths.windowsDir + format + "Parser.exe" + "\" " + params + " \"" + file.getAbsoluteFile() + "\""
	    val process: Process = sys.runtime.exec(command)    
	    
	    // Waiting until the end of the command execution
	    if(process.waitFor() != 0) sys.error("Can't parse pdf file. Program will exit")
	    
	    new File(file.getParent() + "\\" + FileFormatDispatcher.name(file.getName) + "." + format)
      }
      else file
	 }
  }
}

case class TXTFormat (file: File) extends FileFormat

case class PDFFormat (file: File) extends FileFormat

object Paths {
  val toolsDir = "tools\\"
  val linuxDir = "linux\\"
  val windowsDir = "windows\\"
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
	def ext(file: String) = """\..+$""".r.findFirstIn(file).get
	def name(file: String) = """\..+$""".r.replaceAllIn(file, "")
	private val supportedFormats = """txt|pdf"""
	
	// Returns the files from a directory (it must be modified if new formats are supported)
	def getFilesFromDirectory(orig: File) = {
	  if(orig.isDirectory()) orig.listFiles.filter(f => (""".+\.(""" + supportedFormats + """)$""").r.findFirstIn(f.getName).isDefined).toList
	  else Nil
	}
	
	// Determines which FileFormat should be used according to the extension of the file
	def getFileFormat(file: File): FileFormat = {
	  
	  ext(file.getName()) match {
	    case ".txt" => new TXTFormat(file)
	    case ".pdf" => new PDFFormat(file)
	  }
	}
	
	// Performs final procedures before the end of the file processing (mostly deleting intermediate files)
	def releaseFile(file: File, format: String) = {
	  ext(file.getName()) match {
	    case ".txt" =>
	    case ".pdf" => {
	      if(Paths.isWindows){
	         val process: Process = sys.runtime.exec("cmd /c del \"" + file.getParent() + "\\" + name(file.getName()) + "." + format + "\"")
	         
	         // Waiting until the end of the command execution
	         if(process.waitFor() != 0) sys.error("Can't release pdf file. Program will exit")
	      }
	    }
	  }
	}
}