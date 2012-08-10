package paper
import java.io.File
import scala.io.Source
import scala.io.BufferedSource

trait FileLoader {
	def loadFromFile(file : File, p : Parsers) : Option[Paper]
	
	// This function sets the id and metadata before final returning
	def setLastModifications(file: File, paper: Option[Paper]): Option[Paper] = {
	  if (paper != None) {
	      // Get index
	      var id = (FileFormatDispatcher.name(file.getName())).toInt
	
	      // Set filename and id
	      val finalPaper : Paper = paper.get.setMeta("file" -> file.getPath).setId(id)
	
	      return Some(finalPaper)
      }
	  return None
	}
}

// This object uses external tools in order to extract text from pdf or other formats.
object ExternalLoader extends FileLoader {
  def loadFromFile(file : File, p : Parsers) : Option[Paper] = {
    println("parsing " + file.getPath + " using external loader")
	loadFromFile(file, p, "txt", "-enc UTF-8")
  }
  
  def loadFromFile(file : File, p : Parsers, format : String, params : String) : Option[Paper] = {    
    // getFileFormat looks for other file formats (like pdf), then using external tools it extracts the content of the file and 
    // puts it in a given format file
    val text = Source.fromFile(FileFormatDispatcher.getFileFormat(file).parseTo(format, params))

    // actual parsing of the file content
    val maybePaper : Option[Paper] = p.parse(text)
    
    // this method deletes temporary files that could have been created previously
    FileFormatDispatcher.releaseFile(file, format)
    
    // If paper exists and parsed, save it in cache
    setLastModifications(file, maybePaper)
  }
}

object XMLParserLoader extends FileLoader {
    def loadFromFile(file : File, p : Parsers) : Option[Paper] = {
    
    // looking for the format and setting the correct parameters
    FileFormatDispatcher.ext(file.getName()) match {
      case ".txt" => ExternalLoader.loadFromFile(file, p)
      case ".pdf" => {
    	  println("parsing " + file.getPath + " using xml loader")
    	  ExternalLoader.loadFromFile(file, p, "xml", "-xml -enc UTF-8")}
    }
  }
}