package paper
import scala.io.Source


object JQParser extends Parsers{
	val chars = """.*"""
	/*
	val introduction = introWord + chars + introEnd
	val references = 
  */
   def extractTitle(in: Source): Title = {
	  Title(in.getLine(1))
	}
	
   def extractReferences(in: Source): List[Reference] = {
     List()
   }
	  
   def extractAbstract(in: Source): Abstract = {
     Abstract("Not saved")
   }
   
   def extractBody(in: Source): Body = {
     Body("Not saved")
   }
   
   def extractAuthors(in: Source): List[Author] = {
     List()
   }
   
	// The function for actually parsing a paper
   def parse(in : Source) : Option[Paper] = {
	  val titleIN = extractTitle(in)
	  val referencesIN = extractReferences(in)
	  val abstractIN = extractAbstract(in)
	  val bodyIN = extractBody(in)
	  val authorsIN = extractAuthors(in)
	  
	  
	  val result = Paper(0, 0, titleIN, authorsIN, abstractIN, bodyIN, referencesIN, Map.empty, List()) 
	  
	  Some(result.setMeta("parsed" -> "yes"))
    }

}