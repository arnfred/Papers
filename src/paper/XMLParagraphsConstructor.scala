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
		val pageCenter = new ParagraphDelimiter(pagePosition.getWidth / 2, 5)

		def createXMLLine(line: xml.Node) = new XMLLine((line \ "@font").text, new XMLPosition((line \ "@left").text.toInt, (line \ "@top").text.toInt, (line \ "@width").text.toInt, (line \ "@height").text.toInt), line.text)
		
		// incorporation rules
		def incorporate(nextLines: NodeSeq, currentLine: XMLLine): (NodeSeq, XMLLine) = nextLines.isEmpty match{
		  case true => (nextLines, currentLine)
		  case false =>
			val nextLine = createXMLLine(nextLines.head)
			val currentLineYRange = new ParagraphDelimiter(currentLine.getPosition.getY, currentLine.getPosition.getHeight)
			val tabulation = if(nextLine.getPosition.getX - (currentLine.getPosition.getX + currentLine.getPosition.getWidth) >= 2*currentLine.getPosition.getHeight) " \t " else " "
	
			// If the top of the next line is in the range, then it must be added to the current line
			if(currentLineYRange === nextLine.getPosition.getY) incorporate(nextLines.tail, currentLine.addText(nextLine.setText(tabulation + nextLine.getText)))
			else (nextLines, currentLine)
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
			
			val previousLineBegin = new ParagraphDelimiter(paragraph.getLines.head.getPosition.getX, 5)
			val previousLineEnd = new ParagraphDelimiter(paragraph.getLines.head.getPosition.getX + paragraph.getLines.head.getPosition.getWidth, 5)
			val lineBegin = line.getPosition.getX
			val lineEnd = line.getPosition.getX + line.getPosition.getWidth
			
			// This is the second paragraph's line (rule 2)
			if(paragraph.getLines.length == 1) {
				if(previousLineEnd === lineEnd) return (paragraph.addLine(line).addOption(XMLParagraphOptions.JUSTIFY), true, true)
				// rule 4
				else if(paragraphCenter === lineCenter && fontsContainer.getXMLFont(paragraph.getFontID).get.checkID(line.getFontID)) return (paragraph.addLine(line).addOption(XMLParagraphOptions.CENTERED), true, true)
				
				else return (paragraph, false, false)
			}
			
			// Normal line (rules 3, 4, 5)
			if(paragraph.getLines.length > 1) {
				// rule 3
				if(previousLineBegin === lineBegin && previousLineEnd === lineEnd) return (paragraph.addLine(line), true, true)
				// rule 4
				else if(paragraphCenter === lineCenter && fontsContainer.getXMLFont(paragraph.getFontID).get.checkID(line.getFontID)) return (paragraph.addLine(line), true, true)
				// rule 5
				else if(previousLineBegin === lineBegin && previousLineEnd > lineEnd) return (paragraph.addLine(line), true, false)
				// rule 6
				else if(previousLineBegin != lineBegin) return (paragraph, false, false)
				// rule 7 to do
				
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
			val incorporation = incorporate(lines.tail, tempLine)

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
		
		
		def processGlobalPage(paragraphs: List[XMLParagraph]): List[XMLParagraph] = paragraphs.remove(p => p.getLines.length == 1 && p.getText.length() <= 3)
		// Reversed and reversed becomes normal
		val finalParagraphs = processGlobalPage(constructParagraphs(lines, List()).reverse)
		
		Some(finalParagraphs)
   }
}