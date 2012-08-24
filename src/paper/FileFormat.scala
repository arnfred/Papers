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
  	  	
	def toolsDir = toolsDirStr + getOSDir
	def ext = if(SystemHelper.isWindows) windowsExtStr else if(SystemHelper.isLinux) linuxExtStr else ""
	def sep = if(SystemHelper.isWindows) windowsSepStr else if(SystemHelper.isLinux) linuxSepStr else ""
}

object Commands {
    private val windowsDelStr = "cmd /c del"
    private val linuxDelStr = "rm -f"
      
    def del = if(SystemHelper.isWindows) windowsDelStr else if(SystemHelper.isLinux) linuxDelStr else ""
}

// This class transforms a File object into another File object, according to the extension of the file
abstract class FileFormat {  
	def convertTo(format: String, params: String): File = this match{
	    case TXTFormat(file) => file
	    case PDFFormat(file) => {
	      val command = "\"" + Paths.toolsDir + "pdfTo" + format + "Parser" + Paths.ext + "\" " + params + " \"" + file.getAbsoluteFile() + "\""
		  val process: Process = sys.runtime.exec(command)    
		    
		  // Waiting until the end of the command execution
		  if(process.waitFor() != 0) { println("Can't parse pdf file. Program will exit"); exit }
	      
		  new File(file.getParent() + Paths.sep + SystemHelper.name(file.getName) + "." + format)
		}
	}
	
	// Performs final procedures before the end of the file processing (mostly deleting intermediate files)
	def releaseFile(format: String) = this match {
	    case TXTFormat(file) =>
	    case PDFFormat(file) => {
	       val process: Process = sys.runtime.exec(Commands.del + " \"" + file.getParent() + Paths.sep + SystemHelper.name(file.getName()) + "." + format + "\"")
	         
	       // Waiting until the end of the command execution
	       if(process.waitFor() != 0) { println("Can't release pdf file. Program will exit"); exit }
	    }
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