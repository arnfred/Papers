package paper
import java.io.File

object Paths {
  	private val toolsDirStr = "tools"
	private val linuxDirStr = "linux"
	private val windowsDirStr = "windows"
	private val windowsSepStr = "\\"
	private val linuxSepStr = "/"
	private val windowsExtStr = ".exe"
	private val linuxExtStr = ""
	  
	  
  	private def getOSDir: String = { if(SystemHelper.isWindows) windowsSepStr + windowsDirStr + windowsSepStr
  	  						 		 else if(SystemHelper.isLinux) linuxSepStr + linuxDirStr + linuxSepStr
  	  						 		 else "" }
  	
  	// Returns the tools directory according to the current operating system
	def toolsDir = toolsDirStr + getOSDir
	
	// Returns the file extension according to the current operating system
	def ext = if(SystemHelper.isWindows) windowsExtStr else if(SystemHelper.isLinux) linuxExtStr else ""
	  
	// Returns the path separator according to the current operating system
	def sep = if(SystemHelper.isWindows) windowsSepStr else if(SystemHelper.isLinux) linuxSepStr else ""
}

// This class transforms a File object into another File object, according to the extension of the file
abstract class FileFormat {  
	def convertTo(format: String, params: List[String]): File = this match{
	    case TXTFormat(file) => file
	    case PDFFormat(file) => {
	      val command = CommandDetector.detect(Paths.toolsDir + "pdfTo" + format + "Converter" + Paths.ext, format)

	      val process: Process = sys.runtime.exec((List(command) ::: params ::: List(file.getAbsolutePath())).toArray[String])
	      
		  // Waiting until the end of the command execution
		  if(process.waitFor() != 0) { println("Can't convert pdf file. Program will exit"); exit }

		  new File(file.getParent() + Paths.sep + SystemHelper.name(file.getName) + "." + format)
		}
	}
	
	// Performs final procedures before the end of the file processing (mostly deleting intermediate files)
	def releaseFile(newFile: File) = this match {
	    case TXTFormat(file) =>
	    case PDFFormat(file) => newFile.delete()
	}
}

case class TXTFormat (file: File) extends FileFormat
case class PDFFormat (file: File) extends FileFormat


object FileFormatDispatcher {	
	// Determines which FileFormat should be used according to the extension of the file
	def getFileFormat(file: File): FileFormat = {
	  SystemHelper.ext(file.getName()) match {
	    case "txt" => new TXTFormat(file)
	    case "pdf" => new PDFFormat(file)
	  }
	}
}

// This object handles special tool cases
object CommandDetector {
	def detect(toolPath: String, format: String): String = {
		if(format.equals("xml") && SystemHelper.isLinux) return "pdftohtml"
		
		toolPath
	}
}