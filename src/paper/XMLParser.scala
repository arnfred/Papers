package paper
import scala.io.Source
import scala.xml.XML
import scala.xml.Elem
import scala.xml.NodeSeq
import scala.xml.TypeSymbol
import scala.util.matching.Regex.MatchIterator


object XMLParser extends Parsers {
	def processAdjacentParagraphs(paragraphs: List[XMLParagraph], condition: (XMLParagraph, List[XMLParagraph]) => Boolean, replacement:(List[XMLParagraph]) => List[XMLParagraph]) = {
		def process(paragraphs: List[XMLParagraph], accu: List[XMLParagraph], cache: List[XMLParagraph]): List[XMLParagraph] = paragraphs match {
		  case List() => accu
		  case x::xs =>
		    // Paragraph feature matched
		    if(condition(x, cache)) process(xs, accu, x::cache)
		    // Paragraph not matched but anything in the cache (normal text)
		    else if(cache.length == 0) process(xs, x::accu, cache)
		    // Paragraph not matched but something in the cache
		    else process(xs, x::replacement(cache.reverse).reverse:::accu, List())
		}
		
		process(paragraphs, List(), List()).reverse
	}
	
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
  
  
   /*
   def extractReferences(t : (XMLDocument, Option[Paper])): (XMLDocument, Option[Paper]) = {
     def extract(xml: Elem, paper: Paper): (Elem, Option[Paper]) = {
        def parseReference(ref: String): Reference = {
           val title = """[“\"].+[”\"]""".r.findFirstIn(ref)
           
           
           Reference(List(), Title(title.get.tail.dropRight(1)))
        }
       
        def extractReferencesFromIterator(iter: MatchIterator, accu: List[Reference]): List[Reference] = iter.hasNext match {
          case false => accu
          case true => extractReferencesFromIterator(iter, parseReference(iter.next())::accu)
        }
        
        
        val pages = xml \\ "page"
        val page = pages.last
        
        val parag = extractParagraph(page, (lines: NodeSeq) => lines.head.text == "R" && lines.tail.head.text == "EFERENCES", (l: NodeSeq) => true)
        
        val iter = """\[[0-9]+\][^\[]+""".r.findAllIn(parag)
        
        val refs = extractReferencesFromIterator(iter, List()).reverse
        
        (xml, Some(paper.setReferences(refs)))
     }
     
     val xml = t._1
     val paper = t._2
     
     if(paper == None) (xml, None)
     else extract(xml, paper.get)
   }
   */
   
   // This method extracts the title of the article.
   // It uses the following rules:
   // - The title is contained in the first page
   // - It has the biggest font size
   def extractTitle(t : (XMLDocument, Option[Paper])): (XMLDocument, Option[Paper]) = {
     val xml = t._1
     val paper = t._2
     
     // This method finds the font id having the maximum size
     def findMaxSizeID(paragraphs: List[XMLParagraph]): String = {
       def findMaxSizeID0(paragraphs: List[XMLParagraph], max: Int, id: String): String = paragraphs match{
         case List() => id
         case x::xs => 
           val newSize = xml.getFontsContainer.getXMLFont(x.getFontID).get.getSize.toInt
           if(newSize > max) findMaxSizeID0(xs, newSize, xml.getFontsContainer.getXMLFont(x.getFontID).get.getID)
           else findMaxSizeID0(xs, max, id)
       }
       
       findMaxSizeID0(paragraphs, 0, "")
     }
     

     
     def extract(p: Paper): Option[Paper] = {
         val maxSizeIDFont = xml.getFontsContainer.getXMLFont(findMaxSizeID(xml.getPage(1).getParagraphs))
         
         val title = xml.getPage(1).getParagraphs.filter(p => maxSizeIDFont.get.checkID(p.getFontID))
         
         if(title.length == 1) Some(p.setTitle(title.head.getText.replace("\n", " ")))
         else None
     }
     
     paper match {
       case None => (xml, None)
       case Some(p) => (xml, extract(p))
     }
     
   }
   /*
   // This method extracts the abstract of the article.
   // It uses the following rules:
   // - The abstract begins by "Abstract-"
   // - It is contained entirely in the first page
   // - The paragraph has the same font type (id)
   def extractAbstract(t : (XMLDocument, Option[Paper])): (XMLDocument, Option[Paper]) = {
	 val abstractRegex = """^Abstract[—-]"""
     
     def extract(xml: Elem, p: Paper): (Elem, Option[Paper]) = {
       // let's find the page with number = 1
	   val pages = (xml \\ "page") filter((n) => (n \ "@number").text == "1")
	   val xmlPageFontsComp = XMLObjectsManager.constructXMLDocument(xml).get.getFontsContainer
	     
	   // there must be only one page
	   if(pages.length != 1 || xmlPageFontsComp == None) { println("Parsing error : other than one page have number 1. Abstract not parsed"); return (xml, None) }
	   else {
	      val firstPage = pages.head
       
	      // let's find the text containing "Abstract-" and store his font id
	      val abstractBegin = (firstPage \\ "text") filter ((t) => (abstractRegex + """.*$""").r.findFirstIn(t.text).isDefined)
	      
	      if(abstractBegin.length != 1) { println("Parsing error : other than one text line is the beginning of the abstract"); (xml, None) }
	      else {
	        val id = (abstractBegin.head \ "@font").text
	        
		    // call of the extractParagraph method, then deleting the beginning ("Abstract—")
	        val xmlFont = xmlPageFontsComp.getXMLFont(id)
	        def f (lines: NodeSeq) = xmlFont.get.checkID((lines.head \\ "@font").text)
	        
	        if(xmlFont == None) (xml, None)
	        else (xml, Some(p.setAbstract(Abstract(abstractRegex.r.replaceAllIn(extractParagraph(firstPage, f, f), "")))))
	      }
       
	   }
     }
     
     val xml = t._1
     val paper = t._2
     
     if(paper == None) (xml, None)
     else extract(xml, paper.get)
   }
   */
	// The function for actually parsing a paper
   def parse(in: Source) : Option[Paper] = {  
	  val xml = getXMLObject(in)
      
	  if(xml == None) None
	  else {
		  val cleanPaper = Paper(0, 0, Title(""), Nil, Abstract("Not saved"), Body("Not saved"), List(), Map.empty, List())
		  val xmlDocument = XMLObjectsManager.constructXMLDocument(xml.get, "\n")
		  
		  if(xmlDocument == None) return None
		  val paper = extractTitle((xmlDocument.get, Some(cleanPaper)))
		  
	      if(paper._2 == None) None
	      else	Some(paper._2.get.setMeta("parsed" -> "yes"))
	  }
   }

}