package paper
import scala.io.Source
import scala.xml.XML
import scala.xml.Elem
import scala.xml.NodeSeq


object XMLParser extends Parsers{
   def getXMLObject(in: Source): Elem = {
      val text = in.mkString
      in.close
      
      XML.loadString("""</?[bi]>""".r.replaceAllIn(text, ""))
   }
  
   // This method extracts the title of the article.
   // It uses the following rules:
   // - The title is contained in the first page
   // - It has the biggest font size
   def extractTitle(xml: Elem): Title = {
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
     
     // let's find the page with number = 1
     val pages = (xml \\ "page") filter((n) => (n \ "@number").text == "1")
     
     // there must be only one page
     if(pages.length != 1) error("") // to complete
     else {
        val firstPage = pages.head
        
        // let0s find the maximum size of fontspec list contained in the first page
        val maxSize = findMaxSize(firstPage \\ "fontspec").toString()
        
        // we find the fontspec node which has the maximum font size
        val maxIdFonts = (firstPage \\ "fontspec") filter ((f) => (f \ "@size").text == maxSize)
        
        // there must be only one font (the title font)
        if(maxIdFonts.length != 1) error("") // to complete
        else {
          // let's find the id of the font
          val id = (maxIdFonts.head \ "@id").text
          // let's find all the texts which have the title font and concatenate them
          val texts = (firstPage \\ "text") filter ((p) => (p \ "@font").text == id)
          Title(concatTexts(texts))
        }
     }  
     
   }
  
	// The function for actually parsing a paper
   def parse(in: Source) : Option[Paper] = {  
	  val xml = getXMLObject(in)
	  
	  val xmlTitle = extractTitle(xml)
	  
      
      val result = Paper(0, 0, xmlTitle, Nil, Abstract("Not saved"), Body("Not saved"), List(), Map.empty, List())
	  Some(result.setMeta("parsed" -> "yes"))
    }

}