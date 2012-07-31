package paper
import java.io.File
import scala.io.Source
import scala.io.BufferedSource
import com.itextpdf.text.pdf.PdfReader
import com.itextpdf.text.pdf.parser.LocationTextExtractionStrategy
import com.itextpdf.text.Rectangle
import com.itextpdf.text.pdf.parser.RenderFilter
import com.itextpdf.text.pdf.parser.RegionTextRenderFilter
import com.itextpdf.text.pdf.parser.FilteredTextRenderListener
import com.itextpdf.text.pdf.parser.PdfTextExtractor

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

// This object uses external tools (like pdftotext) in order to extract text from pdf or other formats.
object ClassicLoader extends FileLoader {
  def loadFromFile(file : File, p : Parsers) : Option[Paper] = {
    println("parsing " + file.getPath + " using classic loader")
    
    // getFileFormat looks for other file formats (like pdf), then using external tools it extracts the content of the file and 
    // puts it in a .txt file
    val text = Source.fromFile(FileFormatDispatcher.getFileFormat(file).parseToTXT)

    // actual parsing of the file content
    val maybePaper : Option[Paper] = p.parse(text) // MODIFIED
    
    // this method deletes temporary files that could have been created previously
    FileFormatDispatcher.releaseFile(file)
    
    // If paper exists and parsed, save it in cache
    setLastModifications(file, maybePaper)
  }
}


// This object extracts text from other file formats using the IText library
object ITextLoader extends FileLoader {
  def loadFromFile(file : File, p : Parsers) : Option[Paper] = {
    
    // looking for the format. If standard text, just using ClassicLoader
    FileFormatDispatcher.ext(file.getName()) match {
      case ".txt" => ClassicLoader.loadFromFile(file, p)
      case ".pdf" => loadAndParsePDF(file, p)
    }
  }
  
  // If the format is pdf, then we use IText library
  def loadAndParsePDF(file: File, p: Parsers): Option[Paper] = {
      // setting of parameters
      val reader : PdfReader = new PdfReader(file.getAbsolutePath());
      val rect : Rectangle = new Rectangle(20, 30, 572, 732)
      
      val filter : RenderFilter = new RegionTextRenderFilter(rect);
      val strategy : FilteredTextRenderListener = new FilteredTextRenderListener(new LocationTextExtractionStrategy(), filter);
    
        // This methods extracts text from a pdf file, using the library
      def load(file: File): Source = {
	      // method for loading a page, uses recursion
	      def loadPage(pagesLimit: Int, pageNo : Int, accu : String): String = {
	        if(pageNo <= pagesLimit) loadPage(pagesLimit, pageNo + 1, accu + PdfTextExtractor.getTextFromPage(reader, pageNo, strategy))
	        else accu
	      }
        
      	  val text = loadPage(reader.getNumberOfPages(), 1, "")
      	println("\n\n\n\n\n" + text + "\n\n\n\n\n\n")
      	  Source.fromString(text)
      }
      
      
	  println("parsing " + file.getPath + " using itext loader")
      val paper = p.parse(load(file))
      
      // Link importation to do
      
	  setLastModifications(file, paper)
  }


}