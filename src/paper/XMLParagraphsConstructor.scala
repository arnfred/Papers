package paper
import scala.xml.NodeSeq

object XMLParagraphsConstructor {
	private class ParagraphDelimiter(value: Int, tolerance: Int) {			  
		// !!! Equal to =   (assignment, not comparing)
		def == (cValue: Int): ParagraphDelimiter = new ParagraphDelimiter(cValue, tolerance)
		
		def >= (cValue: Int): Boolean = (cValue <= value + tolerance)
		def <= (cValue: Int): Boolean = (cValue >= value - tolerance)
		def > (cValue: Int): Boolean = (cValue < value + tolerance)
		def < (cValue: Int): Boolean = (cValue > value - tolerance)
		
		// Comparing
		def === (cValue: Int): Boolean = this >= cValue && this <= cValue
		def != (cValue: Int): Boolean = !(this === cValue)
	}
   
	
	def constructXMLParagraphs(page: xml.Node, pagePosition: XMLPosition, fontsContainer: XMLFontsContainer, lineSeparator: String): Option[List[XMLParagraph]] = {
		val lines = page \\ "text"
		val pageCenter = new ParagraphDelimiter(pagePosition.getWidth / 2, 3)

		def createXMLLine(line: xml.Node) = new XMLLine((line \ "@font").text, new XMLPosition((line \ "@left").text.toInt, (line \ "@top").text.toInt, (line \ "@width").text.toInt, (line \ "@height").text.toInt), line.text)
		
		// This method incorporates the lines and finds out the real line font (see documentation)
		def incorporate(nextLines: NodeSeq, currentLine: XMLLine, fontLengthMap: Map[String, Int]): (NodeSeq, XMLLine) = nextLines.isEmpty match{
		  case true => { 
		      // Determining of the real line font 
			  val maxFontLength = fontLengthMap.toList.map((f:(String, Int)) => f._2).max
			  (nextLines, currentLine.setFontID(fontLengthMap.filter((p:(String, Int)) => p._2 == maxFontLength).head._1))
			}
		  case false =>
			val nextLine = createXMLLine(nextLines.head)
			val currentLineYRange = new ParagraphDelimiter(currentLine.getPosition.getY, currentLine.getPosition.getHeight - 3)
			val tabulation = if(nextLine.getPosition.getX - (currentLine.getPosition.getX + currentLine.getPosition.getWidth) >= 2*currentLine.getPosition.getHeight) " \t " else " "
	
			
			// If the top of the next line is in the range, then it must be added to the current line
			if(currentLineYRange === nextLine.getPosition.getY) { 
			  // Counting of the characters having a particular font
			  val nextFontLength = fontLengthMap.get(nextLine.getFontID)
			  // If a font is always present in the map, then add to the current value the number of characters, otherwise create a new font key
			  val newMap = if(nextFontLength == None) fontLengthMap + ((nextLine.getFontID, (tabulation + nextLine.getText).length)) else fontLengthMap - (nextLine.getFontID) + ((nextLine.getFontID, (tabulation + nextLine.getText).length + nextFontLength.get))
			    
			  incorporate(nextLines.tail, currentLine.addText(nextLine.setText(tabulation + nextLine.getText)), newMap) 
			}
			else {
			  // Determining of the real line font 
			  val maxFontLength = fontLengthMap.toList.map((f:(String, Int)) => f._2).max
			  (nextLines, currentLine.setFontID(fontLengthMap.filter((p:(String, Int)) => p._2 == maxFontLength).head._1))
			}
		}
		
		// This is the main step of the process (see documentation). The output of the method is (newParagraph, line included?, paragraph will continue?)
		def linkLine(line: XMLLine, paragraph: XMLParagraph): (XMLParagraph, Boolean, Boolean) = {		  
			def constructEnumFormat(line: String): String = {
				val enumRegex = """^([^0-9]*)?[0-9]+([^0-9]+?) """
				  
				if(line.length < 5) return ""
				else {
					val result = enumRegex.r.findFirstIn(line.take(5))
					if(result.isDefined) return """[0-9]""".r.replaceAllIn(result.get, "{d}").dropRight(1)
					else return ""
				}
			}
		  
			val lineCenter = ((2 * line.getPosition.getX) + line.getPosition.getWidth) / 2
			val enumFormat = constructEnumFormat(line.getText)
			
			// First paragraph's line (rule 1)
			if(paragraph.getLines.length == 0) {
				val optionContainer = if(pageCenter === lineCenter) (new XMLParagraphOptionsContainer(XMLParagraphOptions.NONE)).addOption(XMLParagraphOptions.PAGE_CENTERED) else new XMLParagraphOptionsContainer(XMLParagraphOptions.NONE)
				val enumeratedOptionContainer = if(enumFormat != "") optionContainer.addOption(XMLParagraphOptions.ENUMERATION) else optionContainer
				
				return (new XMLParagraph(line.getFontID, line.getPosition, enumeratedOptionContainer, List(line), lineSeparator, line.getText, enumFormat), true, true)
			}
			
			// Calculate some important parameters
			val previousLineCenter = new ParagraphDelimiter(((2 * paragraph.getLines.head.getPosition.getX) + paragraph.getLines.head.getPosition.getWidth) / 2, 3)
			val previousLineText = paragraph.getLines.head.getText
			val previousLineTop = paragraph.getLines.head.getPosition.getY
			val previousLineHeight = paragraph.getLines.head.getPosition.getHeight
			val previousLineBegin = new ParagraphDelimiter(paragraph.getLines.head.getPosition.getX, 3)
			val previousLineEnd = new ParagraphDelimiter(paragraph.getLines.head.getPosition.getX + paragraph.getLines.head.getPosition.getWidth, 3)
			val lineBegin = line.getPosition.getX
			val lineEnd = line.getPosition.getX + line.getPosition.getWidth
			val lineTop = line.getPosition.getY
			val lineHeight = line.getPosition.getHeight
			val previousLineCapitalVersurNotDifference = """[A-Z]""".r.findAllIn(previousLineText).length - """[a-z]""".r.findAllIn(previousLineText).length
			val lineCapitalVersurNotDifference = """[A-Z]""".r.findAllIn(line.getText).length - """[a-z]""".r.findAllIn(line.getText).length
			
			// Rule 2
			if(lineTop - previousLineTop > 2*lineHeight || !fontsContainer.getXMLFont(paragraph.getFontID).get.checkID(line.getFontID)) return (paragraph, false, false)
			// Rule 3: The current line is not a title but previous is (contains only capital letters), even if the have the same font
			if(lineCapitalVersurNotDifference <= 0 && previousLineCapitalVersurNotDifference > 0) return (paragraph, false, false)
			// If both lines have the same enumeration format (which must be defined), then the current line is a new enumeration, hence it belongs to a new paragraph
			if(paragraph.hasOption(XMLParagraphOptions.ENUMERATION) && enumFormat == paragraph.getEnumerationFormat) return (paragraph, false, false)
			
			
			// This is the second paragraph's line
			if(paragraph.getLines.length == 1) {
				// Rule 4
				if(previousLineBegin === lineBegin && previousLineEnd === lineEnd && previousLineCenter === lineCenter) return (paragraph.addLine(line).addOption(XMLParagraphOptions.CENTERED).addOption(XMLParagraphOptions.JUSTIFY), true, true)
				// Rule 5
				else if(previousLineBegin != lineBegin && previousLineEnd != lineEnd && previousLineCenter === lineCenter) return (paragraph.addLine(line).addOption(XMLParagraphOptions.CENTERED), true, true)
				// Rule 6
				else if(previousLineEnd >= lineEnd) return (paragraph.addLine(line).addOption(XMLParagraphOptions.JUSTIFY), true, true)
			}
			
			// Normal line (more than the second)
			if(paragraph.getLines.length > 1) {
				if(paragraph.hasOption(XMLParagraphOptions.CENTERED) && !paragraph.hasOption(XMLParagraphOptions.JUSTIFY)) {
					// Rule 7.a
					if(previousLineCenter === lineCenter) return (paragraph.addLine(line), true, true)
				}
				else if(!paragraph.hasOption(XMLParagraphOptions.CENTERED) && paragraph.hasOption(XMLParagraphOptions.JUSTIFY)) {
					// Rule 8.a
					if(previousLineBegin === lineBegin && previousLineEnd === lineEnd) return (paragraph.addLine(line), true, true)
					// Rule 8.b
					else if(previousLineBegin === lineBegin && previousLineEnd > lineEnd) return (paragraph.addLine(line), true, false)
				}
				else if(paragraph.hasOption(XMLParagraphOptions.CENTERED) && paragraph.hasOption(XMLParagraphOptions.JUSTIFY)) {
					// Rule 9.a
					if(previousLineBegin === lineBegin && previousLineEnd === lineEnd) return (paragraph.addLine(line), true, true)
					// Rule 9.b
					else if(previousLineBegin != lineBegin && previousLineEnd != lineEnd && previousLineCenter === lineCenter) return (paragraph.addLine(line).removeOption(XMLParagraphOptions.JUSTIFY), true, true)
					// Rule 9.c
					else if(previousLineBegin === lineBegin && previousLineEnd > lineEnd) return (paragraph.addLine(line).removeOption(XMLParagraphOptions.CENTERED), true, false)
				}
			}
			
			// For all other cases
			(paragraph, false, false)
		}
		
		
		// This method applies the layout rules.
		def setLayout(paragraph: XMLParagraph): XMLParagraph = {
			if(pageCenter > paragraph.getPosition.getX + paragraph.getPosition.getWidth) paragraph.addOption(XMLParagraphOptions.COLUMN_LEFT)
		    else if(pageCenter < paragraph.getPosition.getX) paragraph.addOption(XMLParagraphOptions.COLUMN_RIGHT)
		    else paragraph.addOption(XMLParagraphOptions.NO_COLUMN)
		}
		
		def constructNewParagraph(lines: NodeSeq, currentParagraph: XMLParagraph): (NodeSeq, XMLParagraph) = lines.isEmpty match{
		  case true => (lines, currentParagraph)
		  case false =>
			val tempLine = createXMLLine(lines.head)			
			// Incorporation
			val incorporation = incorporate(lines.tail, tempLine, Map((tempLine.getFontID, tempLine.getText.length)))

			val remainingLines = incorporation._1
			// Linkage
			val linkage = linkLine(incorporation._2, currentParagraph)

			// If the line was added
			val finalRemainingLines = if(linkage._2 == true) remainingLines else lines
			
			// If the paragraph continues
			if(linkage._3 == true) constructNewParagraph(finalRemainingLines, linkage._1)
			else (finalRemainingLines, linkage._1)
		}
     
		// This method constructs the paragraphs. Be careful because the final list is reversed
		def constructParagraphs(lines: NodeSeq, accu: List[XMLParagraph]): List[XMLParagraph] = lines.isEmpty match {
		  case true => accu
		  case false =>
		    val construction = constructNewParagraph(lines, XMLObjectsManager.getCleanXMLParagraph(lineSeparator))
		    val layoutParagraph = setLayout(construction._2.reverseLines)
		    constructParagraphs(construction._1, layoutParagraph :: accu)
		}
		
		// Global page processing
		def processGlobalPage(paragraphs: List[XMLParagraph]): List[XMLParagraph] = paragraphs.remove(p => (p.getLines.length == 1 && p.getText.length() <= 3) || p.getPosition.getX < 0 || (p.getPosition.getX + p.getPosition.getWidth) > (pagePosition.getX + pagePosition.getWidth) || p.getPosition.getY < 0 || (p.getPosition.getY + p.getPosition.getHeight) > (pagePosition.getY + pagePosition.getHeight))
		
		val finalParagraphs = processGlobalPage(constructParagraphs(lines, List()).reverse)
		
		Some(finalParagraphs)
   }
}