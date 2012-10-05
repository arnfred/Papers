package paper

trait InformationExtractor {	
   // This method finds the first element in a list matching a particular condition and returns it with the rest of the list
   protected def findFirstOf(list: List[XMLParagraph], condition: (XMLParagraph) => Boolean): List[XMLParagraph] = list match{
     case List() => Nil
     case x::xs => if(condition(x)) x::xs else findFirstOf(xs, condition)
   }
   
   // This method find the first element matching a condition and returns the previous ones (not matching)
   protected def untilFirstOf(list: List[XMLParagraph], condition: (XMLParagraph) => Boolean): List[XMLParagraph] = {
	   def untilFirstOf0(l: List[XMLParagraph], accu: List[XMLParagraph]): List[XMLParagraph] = l match{
	     case List() => accu.reverse
	     case x::xs => if(condition(x)) accu.reverse else untilFirstOf0(xs, x::accu)
	   }
	   
	   untilFirstOf0(list, List())
   }
   
   // This method filters the first elements matching a condition which are adjacent
   protected def filterAdjacents(list: List[XMLParagraph], condition: (XMLParagraph) => Boolean): List[XMLParagraph] = {
	   def filterAdjacents0(l: List[XMLParagraph], accu: List[XMLParagraph]): List[XMLParagraph] = l match {
	     case List() => accu
	     case x::xs => if(condition(x)) filterAdjacents0(xs, x::accu)
	     			   else if(accu.length == 0) filterAdjacents0(xs, accu) else accu
	   }
	   
	   filterAdjacents0(list, List()).reverse
   }
   
   // This method finds the first elements matching a condition which are adjacent and returns the list of the remaining ones (after these)
   protected def discardAdjacents(list: List[XMLParagraph], condition: (XMLParagraph) => Boolean): List[XMLParagraph] = {
	   def discardAdjacents0(l: List[XMLParagraph], accu: Int): List[XMLParagraph] = l match {
	     case List() => Nil
	     case x::xs => if(condition(x)) discardAdjacents0(xs, accu + 1)
	     			   else if(accu == 0) discardAdjacents0(xs, accu) else x::xs
	   }
	   
	   discardAdjacents0(list, 0)
   }
   
   // This method takes a list of strings and returns a string containing the concatenation of all the other strings
   protected def concatText(list: List[String]): String = {
	   def concatText0(l: List[String], accu: String): String = l match {
	     case List() => if(accu.length() > 0) accu.dropRight(1) else accu // Last \n replacement
	     case x::xs => concatText0(xs, accu + x + "\n")
	   }
	   
	   concatText0(list, "")
   }
}




trait AuthorsExtractor1 extends InformationExtractor{
    // This method extracts the authors of the article.
    // It uses the following rules:
    // - They are located in the first page between title and abstract
    // - Every author paragraph contains the name at the top of it, i.e. the names are located at the first line of these paragraphs
	def extractAuthors(paper: Paper, xml: XMLDocument, paragraphs: List[XMLParagraph]): (List[XMLParagraph], Paper) = {
		val list = untilFirstOf(paragraphs, (p: XMLParagraph) => p.hasOption(XMLParagraphOptions.JUSTIFY) && p.getPosition.getWidth >= xml.getPage(1).getPosition.getWidth / 3)
		val remainingList = paragraphs.drop(list.length)
		
		// Finding of the most at the top paragraphs
		if(list.length == 0) return (paragraphs, paper)
		val minTop = list.map((p:XMLParagraph) => p.getPosition.getY).min
		val unprocessedAuthorsList = list.filter((p:XMLParagraph) => p.getPosition.getY == minTop)
		
		// Applying the extraction processing
		val authors = unprocessedAuthorsList.flatMap((p:XMLParagraph) => ("""(.+?""" + ExtractionRegexes.authorsSeparator + """)|(.+?$)""").r.findAllIn(""" [0-9]+""".r.replaceAllIn(p.getLines.head.getText, "")))
		val authorsList = authors.map((s: String) => new Author(ExtractionRegexes.authorsSeparator.r.replaceAllIn(s, "")))
	
		   
		if(authorsList.length != 0) (remainingList, paper.setAuthors(authorsList))
		else (paragraphs, paper)
		   
	}
}

trait AbstractExtractor1 extends InformationExtractor{
    // This method extracts the abstract of the article.
    // It uses the following rules:
    // - It is the first justified paragraph
    // - It is entirely contained in the first page
	def extractAbstract(paper: Paper, xml: XMLDocument, paragraphs: List[XMLParagraph]): (List[XMLParagraph], Paper) = {
		val list = findFirstOf(paragraphs, (p: XMLParagraph) => p.hasOption(XMLParagraphOptions.JUSTIFY) && p.getPosition.getWidth >= xml.getPage(1).getPosition.getWidth / 3)
		val xmlFont = xml.getFontsContainer.getXMLFont(list.head.getFontID)
		val abstractList = filterAdjacents(list, (p: XMLParagraph) => xmlFont.get.checkID(p.getFontID) && p.hasOption(XMLParagraphOptions.JUSTIFY)).map((p: XMLParagraph) => p.getText.replaceAll("\n", " "))
		val remainingList = discardAdjacents(list, (p: XMLParagraph) => xmlFont.get.checkID(p.getFontID) && p.hasOption(XMLParagraphOptions.JUSTIFY))
			
		if(abstractList.length != 0) (remainingList, paper.setAbstract(new Abstract(concatText(abstractList))))
		else (paragraphs, paper)
	}
}

trait TitleExtractor1 extends InformationExtractor {
    // This method extracts the title of the article.
    // It uses the following rules:
    // - The title is contained in the first page
    // - It has the biggest font size
	def extractTitle(paper: Paper, xml: XMLDocument, paragraphs: List[XMLParagraph]): (List[XMLParagraph], Paper) = {
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
		
		val maxSizeIDFont = xml.getFontsContainer.getXMLFont(findMaxSizeID(paragraphs.take(10)))
        val title = findFirstOf(paragraphs, p => maxSizeIDFont.get.checkID(p.getFontID) || p.hasOption(XMLParagraphOptions.PAGE_CENTERED))

        if(title.length != 0) (title.tail, paper.setTitle(new Title(title.head.getText.replace("\n", " "))))
        else (paragraphs, paper)
	}
}


trait BodyExtractor1 extends InformationExtractor {
    // This method extracts the body of the article.
    // It uses the following rules:
    // - It begins with the first justified paragraph after the abstract
    // - It is justified and belongs to one column
	def extractBody(paper: Paper, xml: XMLDocument, paragraphs: List[XMLParagraph]): (List[XMLParagraph], Paper) = {
		val firstBodyList = findFirstOf(paragraphs, (p: XMLParagraph) => p.hasOption(XMLParagraphOptions.JUSTIFY) && !p.hasOption(XMLParagraphOptions.NO_COLUMN))
		val bodyXmlFont = xml.getFontsContainer.getXMLFont(firstBodyList.head.getFontID)
        
		// Filter (applying the rules)
		val bodyListWithReturn = firstBodyList.filter((p: XMLParagraph) => bodyXmlFont.get.checkID(p.getFontID) && p.hasOption(XMLParagraphOptions.JUSTIFY) && !p.hasOption(XMLParagraphOptions.NO_COLUMN))
		
		// Calculating the remaining list
		val lastBodyParagraph = bodyListWithReturn.last
		val bodyList = bodyListWithReturn.map((p: XMLParagraph) => p.getText.replaceAll("\n", " "))
        val remainingList = findFirstOf(paragraphs, (p:XMLParagraph) => p.getText.equals(lastBodyParagraph.getText)).tail
         
		if(bodyList.length != 0) (remainingList, paper.setBody(new Body(concatText(bodyList))))
		else (paragraphs, paper)
	}
}

trait ReferencesExtractor1 extends InformationExtractor {
    // This method extracts the references of the article.
    // It uses the following rules:
    // - They begin with a paragraph containing a reference title
    // - Each following paragraph is a reference.
    // - The method recognizes the reference format and applies a suitable extraction strategy
	def extractReferences(paper: Paper, xml: XMLDocument, paragraphs: List[XMLParagraph]): (List[XMLParagraph], Paper) = {
        val refParagraphs = findFirstOf(paragraphs, (p: XMLParagraph) => ("""^""" + ExtractionRegexes.referencesName + """$""").r.findFirstIn(p.getText).isDefined)
        val referencesStringList = if(!refParagraphs.isEmpty) refParagraphs.tail else List()

        // This method takes a list of reference paragraphs and process them
        def makeReferences(refs: List[XMLParagraph], accu: List[Reference]): List[Reference] = refs match {
          case List() => accu
          case x::xs => {
        	  // Using of the good reference processor after a structure recognition
        	  val refExtr = ReferenceProcessorExecuter.extract(x.getText.replaceAll("\n", " "))
        	  val title = refExtr._1
        	  val authors = refExtr._2
        	  
        	  if(title == None || authors == None) makeReferences(xs, accu)
        	  else makeReferences(xs, (new Reference(authors.get, title.get)::accu))
          }
        }
        
        val refList = makeReferences(referencesStringList, List()).reverse
        
    	if(refList.length != 0) (List(), paper.setReferences(refList))
		else (paragraphs, paper)
	}
}