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
	val authorsExtractor: AuthorsExtractor = AuthorsExtractor1
	val abstractExtractor: AbstractExtractor = AbstractExtractor1
	val titleExtractor: TitleExtractor = TitleExtractor1
	val referencesExtractor: ReferencesExtractor = ReferencesExtractor1
	val bodyExtractor: BodyExtractor = BodyExtractor1
	val extractionOrder: List[InformationExtractor] = List(titleExtractor, authorsExtractor, abstractExtractor, bodyExtractor, referencesExtractor)
	
	
	
   // This method returns the xml representation of the text contained in the Source object
   def getXMLObject(in: Source): Option[Elem] = {
	  // String generation and illegal xml character removing
	  val text = in.mkString.replace("" + '\uffff', "")
      // This instruction is important, otherwise the xml file can't be deleted
      in.close
      
      try {
    	  // The replacement of the <b> and <i> tags is important because loadString sometimes generate an exception about these tags
    	  // Of course, some information is lost, but not really an important one
    	  Some(XML.loadString("""</?[bi]>""".r.replaceAllIn(text, "")))
      } catch {
      	case _ => println("Couldn't load the XML file."); None
      }
   }
   
   
   // Method for references extraction following the extraction order
   def extract(extractors: List[InformationExtractor], t : (XMLDocument, Option[Paper], List[XMLParagraph])): (XMLDocument, Option[Paper], List[XMLParagraph]) = {
	   def extract0(extractors: List[InformationExtractor], t : (XMLDocument, Option[Paper], List[XMLParagraph])): (XMLDocument, Option[Paper], List[XMLParagraph]) = {
		   if(extractors.length == 0) return t
	     
	       val input = if(extractors.length == 1) t else extract0(extractors.tail, t)
		   val xml = input._1
		   val paper = input._2
		   val paragraphs = input._3
	     
		   if(paper != None) {
			   // Calling the extraction method of the extractor
			   val extraction = extractors.head.extract(paper.get, xml, paragraphs)
			   return (xml, Some(extraction._2), extraction._1)
		   }
		   
		   (xml, None, paragraphs)
	   }
	   
	   extract0(extractors.reverse, t)
   }
   
   // The function for actually parsing a paper
   def parse(in: Source) : Option[Paper] = {
      val xml = getXMLObject(in)
      
	  if(xml == None) None
	  else {
		  val cleanPaper = Paper(0, 0, Title(""), Nil, Abstract("Not saved"), Body("Not saved"), List(), Map.empty, List())
		  val xmlDocument = XMLObjectsManager.constructXMLDocument(xml.get, "\n")
		  
		  if(xmlDocument == None) return None
		  val paper = extract(extractionOrder, (xmlDocument.get, Some(cleanPaper), xmlDocument.get.getParagraphs))
		    
	      if(paper._2 == None) None
	      else	Some(paper._2.get.setMeta("parsed" -> "yes"))
	  }
   }

}