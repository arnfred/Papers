package paper
import scala.xml.XML
import scala.xml.Elem
import scala.xml.NodeSeq

class XMLFont(IDs:String, size: String, family: String, color: String) {
   def getID: String = IDs
   def getSize: String = size
   def getFamily: String = family
   def getColor: String = color
     
   def addID(id: String): XMLFont = new XMLFont(IDs + "|" + id, size, family, color)
   def checkID(id: String): Boolean = ("^" + IDs + "$").r.findFirstIn(id).isDefined
   def compareXMLFont(font: XMLFont): Boolean = (size == font.getSize && family == font.getFamily && color == font.getColor)
}

class XMLPageFontsComparator(pageNumber: String, fonts: List[XMLFont]){
	def getXMLFont(id: String): Option[XMLFont] = fonts.find(f => f.checkID(id))
	def getPageNumber: String = pageNumber
}

object XMLObjectsManager {
   private var fontPageComparators: List[XMLPageFontsComparator] = List()
   def getFontsComparatorFromPage(xml: Elem, pageNumber: String): Option[XMLPageFontsComparator] = {
	   def getFontsComparatorFromPage0: Option[XMLPageFontsComparator] = {
		   def constructFontLists(fonts: NodeSeq): List[XMLFont] = {
		     def constructFontLists0(fonts: NodeSeq, accu: List[XMLFont]): List[XMLFont] = {
		       def checkIfExist(xmlFont: XMLFont, list: List[XMLFont], matched: Boolean, accu: List[XMLFont]): List[XMLFont] = list match{
		         case List() => if(matched) accu else xmlFont :: accu
		         case x::xs => if(x.compareXMLFont(xmlFont)) checkIfExist(xmlFont, xs, true, x.addID(xmlFont.getID) :: accu)
		         			   else checkIfExist(xmlFont, xs, false, x :: accu)
		       }
		       
		       fonts.isEmpty match {
		           case true => accu
		           case false => {
		             val xmlFont = new XMLFont((fonts.head \ "@id").text, (fonts.head \ "@size").text, (fonts.head \ "@family").text, (fonts.head \ "@color").text)
		             constructFontLists0(fonts.tail, checkIfExist(xmlFont, accu, false, List()))
		           }
		       }
		     }
		     
		     constructFontLists0(fonts, List())
		   }
		   
		   val pages = (xml \\ "page") filter((n) => (n \ "@number").text == pageNumber)
		   
		   if(pages.length != 1) None
		   else {
		     val fonts = pages.head \\ "fontspec"
		
		     Some(new XMLPageFontsComparator(pageNumber, constructFontLists(fonts)))
		   }
	   }
	   
	   val pageComparator = fontPageComparators.find(p => p.getPageNumber == pageNumber)
	   
	   if(pageComparator == None) {
	       val pageC = getFontsComparatorFromPage0
	       
	       if(pageC == None) None
	       else fontPageComparators = pageC.get :: fontPageComparators; pageC
	   }
	   else pageComparator
   }
   
   def dispose = {fontPageComparators = List() }
}