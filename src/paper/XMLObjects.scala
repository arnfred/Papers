package paper

// This class represents a position (can be a paragraph or even a page)
class XMLPosition(x: Int, y: Int, width: Int, height: Int) {
   def getX: Int = x
   def getY: Int = y
   def getWidth: Int = width
   def getHeight: Int = height
   
   override def toString(): String = "[x= " + x + ", y= " + y + ", width= " + width + ", height= " + height + "]"
}

// This class defines a particular font (with the totality of xml ids defining it)
class XMLFont(IDs:String, size: String, family: String, color: String) {
   def getID: String = IDs
   def getSize: String = size
   def getFamily: String = family
   def getColor: String = color
   
   // This method adds a new xml id to the definition of the font
   def addID(id: String): XMLFont = new XMLFont(IDs + "-" + id, size, family, color)
   
   // This method checks if a particular xml id is defining that font. Use "xmlFont.checkID(myid)" instead of "xmlFont.getID == myid"
   def checkID(id: String): Boolean = ("(^(" + IDs.replace("-", "|") + ")$)|(^(" + IDs.replace("-", "|") + ")-)|(-(" + IDs.replace("-", "|") + ")$)|(-(" + IDs.replace("-", "|") + ")-)").r.findFirstIn(id).isDefined
   
   // This method checks if two xml font are basically the same
   def compareXMLFont(font: XMLFont): Boolean = (size == font.getSize && family == font.getFamily && color == font.getColor)
}

// This class contains the fonts of the document
class XMLFontsContainer(fonts: List[XMLFont]){
	def getXMLFont(id: String): Option[XMLFont] = fonts.find(f => f.checkID(id))
	def filter (f:XMLFont => Boolean) = fonts.filter(f)
}


object XMLParagraphOptions {
	val CENTERED = "CTR"
	val PAGE_CENTERED = "PCT"
	val JUSTIFY = "JFY"
	val COLUMN_LEFT = "CLL"
	val COLUMN_RIGHT = "CLR"
	val NO_COLUMN = "NCL"
	val ENUMERATION = "ENM"
	val NONE = "NON"
}

class XMLParagraphOptionsContainer(value: String) {
	def getValue: String = value
	def addOption(option: String): XMLParagraphOptionsContainer = if(!hasOption(option)) new XMLParagraphOptionsContainer(value + "|" + option) else this
	def hasOption(option: String): Boolean = ("^(" + value + ")$").r.findFirstIn(option).isDefined
	def removeOption(option: String): XMLParagraphOptionsContainer = new XMLParagraphOptionsContainer(value.replace("|" + option, ""))
}

class XMLLine(fontID: String, position: XMLPosition, text: String) {
	def getFontID: String = fontID
	def getPosition: XMLPosition = position
	def getText: String = text
	
	def setFontID(newFont: String) = new XMLLine(newFont, position, text)
	def addText(newLine: XMLLine): XMLLine = new XMLLine(fontID, new XMLPosition(position.getX, position.getY, (newLine.getPosition.getX + newLine.getPosition.getWidth) - position.getX, position.getHeight), text + newLine.getText)
	def setText(newText: String): XMLLine = new XMLLine(fontID, position, newText)
}

// This class represents a paragraph contained in a page
class XMLParagraph(fontID: String, position: XMLPosition, options: XMLParagraphOptionsContainer, lines: List[XMLLine], linesSeparator: String, text: String, enumFormat: String) {
	def getFontID: String = fontID
	def getPosition: XMLPosition = position
	def getText: String = text
	def getLines: List[XMLLine] = lines
	def getEnumerationFormat: String = enumFormat
	
	def addOption(option: String): XMLParagraph = new XMLParagraph(fontID, position, options.addOption(option), lines, linesSeparator, text, enumFormat)
	def hasOption(option: String): Boolean = options.hasOption(option)
	def removeOption(option: String): XMLParagraph = new XMLParagraph(fontID, position, options.removeOption(option), lines, linesSeparator, text, enumFormat)
	def getOptionsValue: String = options.getValue
	
	// This method adds a new XMLLine to the paragraph. However, the added line will be the first one on the list, so pay attention!
	// The text and position arguments will be correctly updated
	def addLine(line: XMLLine): XMLParagraph = new XMLParagraph(fontID, new XMLPosition(Math.min(position.getX, line.getPosition.getX), position.getY, Math.max(position.getX + position.getWidth, line.getPosition.getX + line.getPosition.getWidth) - Math.min(position.getX, line.getPosition.getX), (line.getPosition.getY + line.getPosition.getHeight) - position.getY), options, line :: lines, linesSeparator, text + linesSeparator + line.getText, enumFormat)
	def addParagraph(newParagraph: XMLParagraph): XMLParagraph = new XMLParagraph(fontID, new XMLPosition(Math.min(position.getX, newParagraph.getPosition.getX), position.getY, Math.max(position.getX + position.getWidth, newParagraph.getPosition.getX + newParagraph.getPosition.getWidth) - Math.min(position.getX, newParagraph.getPosition.getX), (newParagraph.getPosition.getY + newParagraph.getPosition.getHeight) - position.getY), options, newParagraph.getLines ::: lines, linesSeparator, text + linesSeparator + newParagraph.getText, enumFormat)
	def reverseLines: XMLParagraph = new XMLParagraph(fontID, position, options, lines.reverse, linesSeparator, text, enumFormat)
}

// This class is the representation of a page
class XMLPage(number: Int, position: XMLPosition, paragraphs: List[XMLParagraph]) {
	def getNumber: Int = number
	def getPosition: XMLPosition = position
	def getParagraphs: List[XMLParagraph] = paragraphs
}

// This class defines the entire xml file structure
class XMLDocument(fontsContainer: XMLFontsContainer, pages: List[XMLPage]) {
	private val paragraphs = {
		def get(pageList: List[XMLPage], accu: List[XMLParagraph]):List[XMLParagraph] = pageList match {
		  case List() => accu
		  case x::xs => get(xs, accu ::: x.getParagraphs)
		}
		
		get(pages, List())
	}
  
	def getFontsContainer: XMLFontsContainer = fontsContainer
	def getPage(pageNumber: Int): XMLPage = (pages.filter(p => p.getNumber == pageNumber)).head
	def getParagraphs: List[XMLParagraph] = paragraphs
}


