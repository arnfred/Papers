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
		
		// incorporation rules
		def incorporate(nextLines: NodeSeq, currentLine: XMLLine, fontLengthMap: Map[String, Int]): (NodeSeq, XMLLine) = nextLines.isEmpty match{
		  case true => {
			  val maxFontLength = fontLengthMap.toList.map((f:(String, Int)) => f._2).max
			  (nextLines, currentLine.setFontID(fontLengthMap.filter((p:(String, Int)) => p._2 == maxFontLength).head._1))
			}
		  case false =>
			val nextLine = createXMLLine(nextLines.head)
			val currentLineYRange = new ParagraphDelimiter(currentLine.getPosition.getY, currentLine.getPosition.getHeight)
			val tabulation = if(nextLine.getPosition.getX - (currentLine.getPosition.getX + currentLine.getPosition.getWidth) >= 2*currentLine.getPosition.getHeight) " \t " else " "
	
			
			// If the top of the next line is in the range, then it must be added to the current line
			if(currentLineYRange === nextLine.getPosition.getY) { 
			  val nextFontLength = fontLengthMap.get(nextLine.getFontID)
			  val newMap = if(nextFontLength == None) fontLengthMap + ((nextLine.getFontID, (tabulation + nextLine.getText).length)) else fontLengthMap - (nextLine.getFontID) + ((nextLine.getFontID, (tabulation + nextLine.getText).length + nextFontLength.get))
			    
			  incorporate(nextLines.tail, currentLine.addText(nextLine.setText(tabulation + nextLine.getText)), newMap) 
			}
			else {
			  val maxFontLength = fontLengthMap.toList.map((f:(String, Int)) => f._2).max
			  (nextLines, currentLine.setFontID(fontLengthMap.filter((p:(String, Int)) => p._2 == maxFontLength).head._1))
			}
		}
		
		// output is (newParagraph, line included?, paragraph will continue?)
		def linkLine(line: XMLLine, paragraph: XMLParagraph): (XMLParagraph, Boolean, Boolean) = {		  
			val lineCenter = ((2 * line.getPosition.getX) + line.getPosition.getWidth) / 2
			val paragraphCenter = new ParagraphDelimiter(((2 * paragraph.getPosition.getX) + paragraph.getPosition.getWidth) / 2, 5)

		  
			// First paragraph's line (rule 1)
			if(paragraph.getLines.length == 0) {
				val optionContainer = if(pageCenter === lineCenter) (new XMLParagraphOptionsContainer(XMLParagraphOptions.NONE)).addOption(XMLParagraphOptions.CENTERED) else new XMLParagraphOptionsContainer(XMLParagraphOptions.NONE)

				return (new XMLParagraph(line.getFontID, line.getPosition, optionContainer, List(line), lineSeparator, line.getText), true, true)
			}
			
			val previousLineText = paragraph.getLines.head.getText
			val previousLineTop = paragraph.getLines.head.getPosition.getY
			val previousLineHeight = paragraph.getLines.head.getPosition.getHeight
			val previousLineBegin = new ParagraphDelimiter(paragraph.getLines.head.getPosition.getX, 3)
			val previousLineEnd = new ParagraphDelimiter(paragraph.getLines.head.getPosition.getX + paragraph.getLines.head.getPosition.getWidth, 3)
			val lineBegin = line.getPosition.getX
			val lineEnd = line.getPosition.getX + line.getPosition.getWidth
			val lineTop = line.getPosition.getY
			val lineHeight = line.getPosition.getHeight
			
			// rule 7
			if(lineTop - previousLineTop > 2*previousLineHeight || !fontsContainer.getXMLFont(paragraph.getFontID).get.checkID(line.getFontID)) return (paragraph, false, false)	
			
			// This is the second paragraph's line
			if(paragraph.getLines.length == 1) {
				// rule 2
				if(previousLineEnd === lineEnd) return (paragraph.addLine(line).addOption(XMLParagraphOptions.JUSTIFY), true, true)
				// rule 4
				else if(paragraphCenter === lineCenter && """[A-Z]""".r.findAllIn(previousLineText).length < """[a-z]""".r.findAllIn(previousLineText).length) return (paragraph.addLine(line).addOption(XMLParagraphOptions.CENTERED), true, true)
				// rule 5
				else if(previousLineEnd > lineEnd && paragraphCenter != lineCenter) return (paragraph.addLine(line).addOption(XMLParagraphOptions.JUSTIFY), true, false)
			}
			
			// Normal line (rules 3, 4, 5)
			if(paragraph.getLines.length > 1) {
				// rule 3
				if(previousLineBegin === lineBegin && previousLineEnd === lineEnd) return (paragraph.addLine(line), true, true)
				// rule 4
				//else if(!paragraph.hasOption(XMLParagraphOptions.JUSTIFY) && paragraphCenter === lineCenter && fontsContainer.getXMLFont(paragraph.getFontID).get.checkID(line.getFontID)) return (paragraph.addLine(line), true, true)
				else if(previousLineBegin != lineBegin && previousLineEnd != lineEnd && paragraphCenter === lineCenter) if(paragraph.hasOption(XMLParagraphOptions.JUSTIFY)) return (paragraph.addLine(line).removeOption(XMLParagraphOptions.JUSTIFY).addOption(XMLParagraphOptions.CENTERED), true, true) else return (paragraph.addLine(line), true, true)
				// rule 5
				else if(previousLineBegin === lineBegin && previousLineEnd > lineEnd) return (paragraph.addLine(line), true, false)
				// rule 6
				else if(previousLineBegin != lineBegin) return (paragraph, false, false)
			}
			
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
			val incorporation = incorporate(lines.tail, tempLine, Map((tempLine.getFontID, tempLine.getText.length)))

			val remainingLines = incorporation._1
			val linkage = linkLine(incorporation._2, currentParagraph)

			// if the line was added
			val finalRemainingLines = if(linkage._2 == true) remainingLines else lines
			
			// if the paragraph continues
			if(linkage._3 == true) constructNewParagraph(finalRemainingLines, linkage._1)
			else (finalRemainingLines, linkage._1)
		}
     
		// This method constructs the paragraphs. Be careful because the final list is reversed
		def constructParagraphs(lines: NodeSeq, accu: List[XMLParagraph]): List[XMLParagraph] = lines.isEmpty match {
		  case true => accu
		  case false =>
		    val construction = constructNewParagraph(lines, new XMLParagraph("", new XMLPosition(0, 0, 0, 0), new XMLParagraphOptionsContainer(XMLParagraphOptions.NONE), List(), lineSeparator, ""))
		    val layoutParagraph = setLayout(construction._2.reverseLines)
		    constructParagraphs(construction._1, layoutParagraph :: accu)
		}
		
		
		def processGlobalPage(paragraphs: List[XMLParagraph]): List[XMLParagraph] = paragraphs.remove(p => (p.getLines.length == 1 && p.getText.length() <= 3) || p.getPosition.getX < 0 || (p.getPosition.getX + p.getPosition.getWidth) > (pagePosition.getX + pagePosition.getWidth) || p.getPosition.getY < 0 || (p.getPosition.getY + p.getPosition.getHeight) > (pagePosition.getY + pagePosition.getHeight))
		// Reversed and reversed becomes normal
		val finalParagraphs = processGlobalPage(constructParagraphs(lines, List()).reverse)
		
		Some(finalParagraphs)
   }
}