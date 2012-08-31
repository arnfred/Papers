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
	
   // This method finds the first element in a list matching a particular condition and returns it with the rest of the list
   def findFirstOf(list: List[XMLParagraph], condition: (XMLParagraph) => Boolean): List[XMLParagraph] = list match{
     case List() => Nil
     case x::xs => if(condition(x)) x::xs else findFirstOf(xs, condition)
   }
   
   // This method find the first element matching a condition and return the previous ones (not matching)
   def untilFirstOf(list: List[XMLParagraph], condition: (XMLParagraph) => Boolean): List[XMLParagraph] = {
	   def untilFirstOf0(l: List[XMLParagraph], accu: List[XMLParagraph]): List[XMLParagraph] = l match{
	     case List() => accu.reverse
	     case x::xs => if(condition(x)) accu.reverse else untilFirstOf0(xs, x::accu)
	   }
	   
	   untilFirstOf0(list, List())
   }
   
   // This method filters some the first elements matching a condition which are adjacent
   def filterAdjacents(list: List[XMLParagraph], condition: (XMLParagraph) => Boolean): List[XMLParagraph] = {
	   def filterAdjacents0(l: List[XMLParagraph], accu: List[XMLParagraph]): List[XMLParagraph] = l match {
	     case List() => accu
	     case x::xs => if(condition(x)) filterAdjacents0(xs, x::accu)
	     			   else if(accu.length == 0) filterAdjacents0(xs, accu) else accu
	   }
	   
	   filterAdjacents0(list, List()).reverse
   }
   
   // This method finds the first elements matching a condition which are adjacent and returns the list of the remaining ones (after these)
   def discardAdjacents(list: List[XMLParagraph], condition: (XMLParagraph) => Boolean): List[XMLParagraph] = {
	   def discardAdjacents0(l: List[XMLParagraph], accu: Int): List[XMLParagraph] = l match {
	     case List() => Nil
	     case x::xs => if(condition(x)) discardAdjacents0(xs, accu + 1)
	     			   else if(accu == 0) discardAdjacents0(xs, accu) else x::xs
	   }
	   
	   discardAdjacents0(list, 0)
   }
   
   def concatText(list: List[String]): String = {
	   def concatText0(l: List[String], accu: String): String = l match {
	     case List() => if(accu.length() > 0) accu.dropRight(1) else accu // Last \n replacement
	     case x::xs => concatText0(xs, accu + x + "\n")
	   }
	   
	   concatText0(list, "")
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
  
  
   
   def extractReferences(t : (XMLDocument, Option[Paper], List[XMLParagraph])): (XMLDocument, Option[Paper], List[XMLParagraph]) = {
     val xml = t._1
     val paper = t._2
     val paragraphs = t._3
     
     def extract(paper: Paper): (Option[Paper], List[XMLParagraph]) = {
        val referencesStringList = findFirstOf(xml.getParagraphs, (p: XMLParagraph) => ("""^""" + ExtractionRegexes.referencesName + """$""").r.findFirstIn(p.getText).isDefined).tail

        def makeReferences(refs: List[XMLParagraph], accu: List[Reference]): List[Reference] = refs match {
          case List() => accu
          case x::xs => {
        	  val refExtr = ReferenceExtractorDispatcher.getReferenceExtractor(x.getText.replaceAll("\n", " "))
        	  val title = refExtr.extractTitle()
        	  val authors = refExtr.extractAuthors()
        	  
        	  if(title == None || authors == None) makeReferences(xs, accu)
        	  else makeReferences(xs, (new Reference(authors.get, title.get)::accu))
          }
        }
        
    	(Some(paper.setReferences(makeReferences(referencesStringList, List()).reverse)), List())
     }
     
     
	   paper match {
		 case None => (xml, None, paragraphs)
		 case Some(p) => {val extraction = extract(p); (xml, extraction._1, extraction._2)}
	   }
   }
   
   
   // This method extracts the bopdy of the article.
   // It uses the following rules:
   // - It begins with the first justified paragraph after the abstract
   // - It is justified and belongs to one column
   def extractBody(t : (XMLDocument, Option[Paper], List[XMLParagraph])): (XMLDocument, Option[Paper], List[XMLParagraph]) = {
     val xml = t._1
     val paper = t._2
     val paragraphs = t._3
     
     def extract(p: Paper): (Option[Paper], List[XMLParagraph]) = {
		 val firstBodyList = findFirstOf(paragraphs, (p: XMLParagraph) => p.hasOption(XMLParagraphOptions.JUSTIFY) && !p.hasOption(XMLParagraphOptions.NO_COLUMN))
		 val bodyXmlFont = xml.getFontsContainer.getXMLFont(firstBodyList.head.getFontID)
         val bodyListWithReturn = firstBodyList.filter((p: XMLParagraph) => bodyXmlFont.get.checkID(p.getFontID) && p.hasOption(XMLParagraphOptions.JUSTIFY) && !p.hasOption(XMLParagraphOptions.NO_COLUMN))
		 val lastBodyParagraph = bodyListWithReturn.last
		 val bodyList = bodyListWithReturn.map((p: XMLParagraph) => p.getText.replaceAll("\n", " "))
         val remainingList = findFirstOf(paragraphs, (p:XMLParagraph) => p.getText.equals(lastBodyParagraph.getText)).tail
         
		 if(bodyList.length > 0) (Some(p.setBody(new Body(concatText(bodyList)))), remainingList)
		 else (None, paragraphs)
     }
     
	   paper match {
		 case None => (xml, None, paragraphs)
		 case Some(p) => {val extraction = extract(p); (xml, extraction._1, extraction._2)}
	   }
   }
   
   
   def extractAuthors(t : (XMLDocument, Option[Paper], List[XMLParagraph])): (XMLDocument, Option[Paper], List[XMLParagraph]) = {
	   val xml = t._1
	   val paper = t._2
	   val paragraphs = t._3
     
	   def extract(p: Paper): (Option[Paper], List[XMLParagraph]) = {
		   val list = untilFirstOf(paragraphs, (p: XMLParagraph) => p.hasOption(XMLParagraphOptions.JUSTIFY) && p.getPosition.getWidth >= xml.getPage(1).getPosition.getWidth / 3)
		   val remainingList = paragraphs.drop(list.length)
		   
		   val authorsList = AuthorsDetector.detect(list)
		   
		   if(authorsList != None) (Some(p.setAuthors(authorsList.get)), remainingList)
		   else (Some(p), paragraphs)
	   }
	   
	   paper match {
		 case None => (xml, None, paragraphs)
		 case Some(p) => {val extraction = extract(p); (xml, extraction._1, extraction._2)}
	   }
   }
   
   
   
   
   // This method extracts the title of the article.
   // It uses the following rules:
   // - The title is contained in the first page
   // - It has the biggest font size
   def extractTitle(t : (XMLDocument, Option[Paper], List[XMLParagraph])): (XMLDocument, Option[Paper], List[XMLParagraph]) = {
     val xml = t._1
     val paper = t._2
     val paragraphs = t._3
     
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
     

     
     def extract(p: Paper): (Option[Paper], List[XMLParagraph]) = {
         val maxSizeIDFont = xml.getFontsContainer.getXMLFont(findMaxSizeID(paragraphs.take(5)))
         val title = findFirstOf(paragraphs, p => maxSizeIDFont.get.checkID(p.getFontID) && p.hasOption(XMLParagraphOptions.CENTERED) && p.hasOption(XMLParagraphOptions.NO_COLUMN))

         if(title.length >= 1) (Some(p.setTitle(title.head.getText.replace("\n", " "))), title.tail)
         else (Some(p), paragraphs)
     }
     
	   paper match {
		 case None => (xml, None, paragraphs)
		 case Some(p) => {val extraction = extract(p); (xml, extraction._1, extraction._2)}
	   }
   }
   
   // This method extracts the abstract of the article.
   // It uses the following rules:
   // - It is the first justified paragraph
   // - It is entirely contained in the first page
   def extractAbstract(t : (XMLDocument, Option[Paper], List[XMLParagraph])): (XMLDocument, Option[Paper], List[XMLParagraph]) = {
	   val xml = t._1
	   val paper = t._2
	   val paragraphs = t._3
     
	   def extract(p: Paper): (Option[Paper], List[XMLParagraph]) = {	     
			val list = findFirstOf(paragraphs, (p: XMLParagraph) => p.hasOption(XMLParagraphOptions.JUSTIFY) && p.getPosition.getWidth >= xml.getPage(1).getPosition.getWidth / 3)
			val xmlFont = xml.getFontsContainer.getXMLFont(list.head.getFontID)
			val abstractList = filterAdjacents(list, (p: XMLParagraph) => xmlFont.get.checkID(p.getFontID) && p.hasOption(XMLParagraphOptions.JUSTIFY)).map((p: XMLParagraph) => p.getText.replaceAll("\n", " "))
			val remainingList = discardAdjacents(list, (p: XMLParagraph) => xmlFont.get.checkID(p.getFontID) && p.hasOption(XMLParagraphOptions.JUSTIFY))
			
			if(abstractList.length > 0) (Some(p.setAbstract(new Abstract(concatText(abstractList)))), remainingList)
			else (Some(p), paragraphs)
	   }
	   
	   paper match {
		 case None => (xml, None, paragraphs)
		 case Some(p) => {val extraction = extract(p); (xml, extraction._1, extraction._2)}
	   }
   }
   
   // The function for actually parsing a paper
   def parse(in: Source) : Option[Paper] = {
      val xml = getXMLObject(in)
      
	  if(xml == None) None
	  else {
		  val cleanPaper = Paper(0, 0, Title(""), Nil, Abstract("Not saved"), Body("Not saved"), List(), Map.empty, List())
		  val xmlDocument = XMLObjectsManager.constructXMLDocument(xml.get, "\n")
		  
		  if(xmlDocument == None) return None
		  val paper = extractReferences(extractBody(extractAbstract(extractAuthors(extractTitle((xmlDocument.get, Some(cleanPaper), xmlDocument.get.getParagraphs))))))
		  
	      if(paper._2 == None) None
	      else	Some(paper._2.get.setMeta("parsed" -> "yes"))
	  }
   }

}