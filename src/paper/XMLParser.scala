package paper
import scala.io.Source
import scala.xml.XML
import scala.xml.Elem
import scala.xml.NodeSeq
import scala.xml.TypeSymbol
import scala.util.matching.Regex.MatchIterator
import sun.nio.cs.Unicode


object XMLParser extends Parsers {
	// These are the modifiable parameters (extractors and extraction order)
	val authorsExtractor = AuthorsExtractor1
	val abstractExtractor = AbstractExtractor1
	val titleExtractor = TitleExtractor1
	val referencesExtractor = ReferencesExtractor1
	val bodyExtractor = BodyExtractor1
	
	def extractInformation(xmlDocument: XMLDocument, cleanPaper: Paper): (XMLDocument, Option[Paper], List[XMLParagraph]) = extractReferences(extractBody(extractAbstract(extractAuthors(extractTitle((xmlDocument, Some(cleanPaper), xmlDocument.getParagraphs))))))
	
	
	
   // This method returns the xml representation of the text contained in the Source object
   def getXMLObject(in: Source): Option[Elem] = {
      val text = in.mkString
      in.close
      
      try {
    	  Some(XML.loadString("""</?[bi]>""".r.replaceAllIn(text, "")))
      } catch {
        case _ => println("Couldn't load the XML file."); None
      }
   }
   
   // Method for references extraction
   def extractReferences(t : (XMLDocument, Option[Paper], List[XMLParagraph])): (XMLDocument, Option[Paper], List[XMLParagraph]) = {
	   val xml = t._1
	   val paper = t._2
	   val paragraphs = t._3
     
	   if(paper != None) {
		   val references = referencesExtractor.extract(xml, paragraphs)
		   if(references._2 != None) return (xml, Some(paper.get.setReferences(references._2.get)), references._1)
	   }
	   
	   (xml, None, paragraphs)
   }
   
   
   // Method for body extraction
   def extractBody(t : (XMLDocument, Option[Paper], List[XMLParagraph])): (XMLDocument, Option[Paper], List[XMLParagraph]) = {
	   val xml = t._1
	   val paper = t._2
	   val paragraphs = t._3
     
	   if(paper != None) {
		   val body = bodyExtractor.extract(xml, paragraphs)
		   if(body._2 != None) return (xml, Some(paper.get.setBody(body._2.get)), body._1)
	   }
	   
	   (xml, None, paragraphs)
   }
   
   // Method for authors extraction
   def extractAuthors(t : (XMLDocument, Option[Paper], List[XMLParagraph])): (XMLDocument, Option[Paper], List[XMLParagraph]) = {
	   val xml = t._1
	   val paper = t._2
	   val paragraphs = t._3
	   
	   if(paper != None) {
		   val authors = authorsExtractor.extract(xml, paragraphs)
		   if(authors._2 != None) return (xml, Some(paper.get.setAuthors(authors._2.get)), authors._1)
	   }
	   
	   (xml, None, paragraphs)
   }
   
   // Method for title extraction
   def extractTitle(t : (XMLDocument, Option[Paper], List[XMLParagraph])): (XMLDocument, Option[Paper], List[XMLParagraph]) = {
     val xml = t._1
     val paper = t._2
     val paragraphs = t._3
     
     if(paper != None) {
    	 val title = titleExtractor.extract(xml, paragraphs)
		 if(title._2 != None) return (xml, Some(paper.get.setTitle(title._2.get)), title._1)
	 }
	   
	 (xml, None, paragraphs)
   }
   
   // Method for abstract extraction
   def extractAbstract(t : (XMLDocument, Option[Paper], List[XMLParagraph])): (XMLDocument, Option[Paper], List[XMLParagraph]) = {
	   val xml = t._1
	   val paper = t._2
	   val paragraphs = t._3
     
	   if(paper != None) {
		   val abstractS = abstractExtractor.extract(xml, paragraphs)
		   if(abstractS._2 != None) return (xml, Some(paper.get.setAbstract(abstractS._2.get)), abstractS._1)
	   }
	   
	   (xml, None, paragraphs)
   }
   
   // The function for actually parsing a paper
   def parse(in: Source) : Option[Paper] = {
      val xml = getXMLObject(in)
      
	  if(xml == None) None
	  else {
		  val cleanPaper = Paper(0, 0, Title(""), Nil, Abstract("Not saved"), Body("Not saved"), List(), Map.empty, List())
		  val xmlDocument = XMLObjectsManager.constructXMLDocument(xml.get, "\n")
	
		  //xmlDocument.get.getParagraphs.foreach((p: XMLParagraph) => println(p.getText + "\n" + p.getOptionsValue + "\n\n"))
		  
		  if(xmlDocument == None) return None
		  val paper = extractInformation(xmlDocument.get, cleanPaper)
		    
	      if(paper._2 == None) None
	      else	Some(paper._2.get.setMeta("parsed" -> "yes"))
	  }
   }

}