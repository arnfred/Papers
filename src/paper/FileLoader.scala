package paper
import java.io.File
import scala.io.Source
import scala.io.BufferedSource

// This object provides helpful methods for file and system managing
object SystemHelper {
	private val supportedFormats = """pdf"""
	private val os = sys.props.get("os.name")
	  
	def ext(file: String) = ("""\..+$""".r.findFirstIn(file).get).tail
	def name(file: String) = """\..+$""".r.replaceAllIn(file, "")
	def isSupported(format: String): Boolean = ("^" + supportedFormats + "$").r.findFirstIn(format).isDefined
	  
	// Returns the files from a directory
	def getFilesFromDirectory(orig: File): List[File] = {
	  if(orig.isDirectory()) orig.listFiles.filter(f => (""".+\.(""" + supportedFormats + """)$""").r.findFirstIn(f.getName).isDefined).toList
	  else Nil
	}
	  
	// The apps is on Windows
	def isWindows: Boolean = os match {
	   case Some(s) => """.*Windows.*""".r.findFirstIn(s.toString()).isDefined
	   case None => false
	}
	  
	// The apps is on Linux
	def isLinux: Boolean = os match {
	   case Some(s) => """.*Linux.*""".r.findFirstIn(s.toString()).isDefined
	   case None => false
	}
}

trait FileLoader {
	def loadFromFile(file : File, p : Parsers) : Option[Paper]
	
	// This function sets the id and metadata before final returning
	def setLastModifications(file: File, paper: Option[Paper]): Option[Paper] = {
	  if (paper != None) {
	      // Get index
		  try {
		      var id = (SystemHelper.name(file.getName())).toInt
		
		      // Set filename and id
		      val finalPaper : Paper = paper.get.setMeta("file" -> file.getPath).setId(id)
		
		      return Some(finalPaper)
		  }catch {
		    case _ => { println("The paper's name must contain only numerical values. Not parsed"); return None }
		  }
      }
	  return None
	}
}

// This loader doesn't care about the format, it just passes the file to the parser
object SimpleLoader extends FileLoader {
  def loadFromFile(file : File, p : Parsers) : Option[Paper] = {
    println("parsing " + file.getPath + " using simple loader")

    val text = Source.fromFile(file)

    // actual parsing of the file content
    val maybePaper : Option[Paper] = p.parse(text)
    
    // If paper exists and parsed, save it in cache
    setLastModifications(file, maybePaper)
  }
}

// This class uses external tools in order to convert a particular format file into another before it is passed to the parser
abstract class ExternalLoader extends FileLoader {  
  // Converts a particular format file into another using an external tool
  def loadFromFile(file : File, p : Parsers, format : String, params : List[String]) : Option[Paper] = {    
    // getFileFormat looks for other file formats (like pdf), then using external tools it extracts the content of the file and 
    // converts it to another format file
    val fileFormat = FileFormatDispatcher.getFileFormat(file)
    val newFile = fileFormat.convertTo(format, params)
    val text = Source.fromFile(newFile)

    // actual parsing of the file content
    val maybePaper : Option[Paper] = p.parse(text)
    
    // this method deletes temporary files that could have been previously created
    fileFormat.releaseFile(newFile)
    
    // If paper exists and parsed, save it in cache
    setLastModifications(file, maybePaper)
  }
}

// This object tries to convert the input file into txt before parsing
object TXTConverterLoader extends ExternalLoader {
    override def loadFromFile(file : File, p : Parsers) : Option[Paper] = {
    	println("parsing " + file.getPath + " using txt loader")
	    // looking for the format and setting the correct parameters
	    loadFromFile(file, p, "txt", List("-enc", "UTF-8"))
    }
}

// This object converts a file into an xml one before parsing. Must use a parser that parses XML
object XMLConverterLoader extends ExternalLoader {
    def loadFromFile(file : File, p : Parsers) : Option[Paper] = {
	    println("parsing " + file.getPath + " using xml loader")
	    
	    // looking for the format and setting the correct parameters
	    SystemHelper.ext(file.getName()) match {
	      case "txt" => loadFromFile(file, p, "txt", List("-enc", "UTF-8"))
	      case "pdf" => loadFromFile(file, p, "xml", List("-xml", "-q", "-enc", "UTF-8"))
	    }
    }
}