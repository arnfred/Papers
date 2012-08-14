package paper
import scala.io.Source
import scala.xml.XML
import scala.xml.Elem
import scala.xml.NodeSeq
import scala.xml.TypeSymbol


object XMLParser extends Parsers {
  
   // This method returns the xml representation of the text contained in the Source object
   def getXMLObject(in: Source): Elem = {
      val text = in.mkString
      in.close
      
      XML.loadString("""</?[bi]>""".r.replaceAllIn(text, ""))
   }
  
   // This method extracts the text contained in A PAGE having a particular font id. 
   // If there are lines with other font id between the paragraph, they will be added
   def extractParagraph(page: xml.Node, xmlFont: Option[XMLFont]): String = {
     // we still need to compare the page's fonts because sometimes more fonts are exactly the same, but have different id
     // and the paragraph pick one of these "randomly"
     // we should maybe construct a structure holding similar fonts
     
     def dropUntil(lines: NodeSeq): NodeSeq = lines.isEmpty match {
        case true => lines
        case false => {
	        if(xmlFont.get.checkID((lines.head \\ "@font").text)) lines
	        else dropUntil(lines.tail)
        }
      }
      
      def extractParagraph0(lines: NodeSeq, accu: String, cache: String): String = lines.isEmpty match {
        case true => if(accu.length() >= 1) accu.take(accu.length - 1) else accu // Avoiding the last space
        case false => {
          if(xmlFont.get.checkID((lines.head \\ "@font").text)) extractParagraph0(lines.tail, accu + cache + lines.head.text + " ", "")
          else extractParagraph0(lines.tail, accu, cache + lines.head.text + " ")
        }				
      }
      
      xmlFont match {
        case None => ""
        case Some(s) => extractParagraph0(dropUntil(page \\ "text"), "", "")
      }
   }
   
   
   // This method extracts the title of the article.
   // It uses the following rules:
   // - The title is contained in the first page
   // - It has the biggest font size
   def extractTitle(t : (Elem, Option[Paper])): (Elem, Option[Paper]) = {
     // This method finds the maximum size of a NodeSeq (of fontspec)
     def findMaxSize(in: NodeSeq): Int = {
       def findMaxSize0(in: NodeSeq, max: Int): Int = {
         if(in.isEmpty) max
         else { 
           val newSize = (in.head \ "@size").text.toInt // here we take the size of a particular fontspec element
           if(newSize > max) findMaxSize0(in.tail, newSize)
           else findMaxSize0(in.tail, max)
         }
       }
       
       findMaxSize0(in, 0)
     }
     
     // This method concatenates a list of texts (in.head.text is the actual text)
     def concatTexts(in: NodeSeq): String = {
       def concatTexts0(in: NodeSeq, accu: String): String = {
         if(in.isEmpty) { if(accu.length() >= 1) accu.take(accu.length - 1) else accu} // we avoid the last space in the concatenation
         else concatTexts0(in.tail, accu + in.head.text + " ") // be aware of the space after the last concatenated string
       }
       
       concatTexts0(in, "")
     }
     
     def extract(xml: Elem, p: Paper): (Elem, Option[Paper]) = {
         // let's find the page with number = 1
	     val pages = (xml \\ "page") filter((n) => (n \ "@number").text == "1")
	     
	     // there must be only one page
	     if(pages.length != 1) { println("Parsing error : other than one page have number 1"); return (xml, None) }
	     else {
	        val firstPage = pages.head
	        
	        // let's find the maximum size of fontspec list contained in the first page
	        val maxSize = findMaxSize(firstPage \\ "fontspec").toString()
	        
	        // we find the fontspec node which has the maximum font size
	        val maxIdFonts = (firstPage \\ "fontspec") filter ((f) => (f \ "@size").text == maxSize)
	        
	        // there must be only one font (the title font)
	        if(maxIdFonts.length != 1) { println("Parsing error : other than one font type has greatest size"); return (xml, None) }
	        else {
	          // let's find the id of the font
	          val id = (maxIdFonts.head \ "@id").text
	          // let's find all the texts which have the title font and concatenate them
	          val texts = (firstPage \\ "text") filter ((p) => (p \ "@font").text == id)
	          (xml, Some(p.setTitle(concatTexts(texts))))
	        }
	     }
     }
     
     val xml = t._1
     val paper = t._2
     
     paper match {
       case None => (xml, None)
       case Some(p) => extract(xml, p)
     }
     
   }
   
   // This method extracts the abstract of the article.
   // It uses the following rules:
   // - The abstract begins by "Abstract-"
   // - It is contained entirely in the first page
   // - The paragraph has the same font type (id)
   def extractAbstract(t : (Elem, Option[Paper])): (Elem, Option[Paper]) = {
	 val abstractRegex = """^Abstract[—-]"""
     
     def extract(xml: Elem, p: Paper): (Elem, Option[Paper]) = {
       // let's find the page with number = 1
	   val pages = (xml \\ "page") filter((n) => (n \ "@number").text == "1")
	     
	   // there must be only one page
	   if(pages.length != 1) { println("Parsing error : other than one page have number 1"); return (xml, None) }
	   else {
	      val firstPage = pages.head
       
	      // let's find the text containing "Abstract-" and store his font id
	      val abstractBegin = (firstPage \\ "text") filter ((t) => (abstractRegex + """.*$""").r.findFirstIn(t.text).isDefined)
	      
	      if(abstractBegin.length != 1) { println("Parsing error : other than one text line is the beginning of the abstract"); (xml, None) }
	      else {
	        val id = (abstractBegin.head \ "@font").text
	        val xmlPageFontsComp = XMLObjectsManager.getFontsComparatorFromPage(xml, "1")
	        
	        xmlPageFontsComp match {
	          case None => (xml, None)
	          case Some(x) =>
		        // call of the extractParagraph method, then deleting the beginning ("Abstract—")
		        (xml, Some(p.setAbstract(Abstract(abstractRegex.r.replaceAllIn(extractParagraph(firstPage, x.getXMLFont(id)), "")))))
		    }
	      }
       
	   }
     }
     
     val xml = t._1
     val paper = t._2
     
     paper match {
       case None => (xml, None)
       case Some(p) => extract(xml, p)
     }
   }
   
	// The function for actually parsing a paper
   def parse(in: Source) : Option[Paper] = {  
	  val xml = getXMLObject(in)
      val cleanPaper = Paper(0, 0, Title(""), Nil, Abstract("Not saved"), Body("Not saved"), List(), Map.empty, List())
	  
	  val paper = extractAbstract(extractTitle((xml, Some(cleanPaper))))
	  XMLObjectsManager.clearFontPageComparatorsList
	  
      paper._2 match {
	    case None => None
	    case Some(p) =>	Some(p.setMeta("parsed" -> "yes"))
	  }
    }

}