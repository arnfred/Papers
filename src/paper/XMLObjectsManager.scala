package paper
import scala.xml.XML
import scala.xml.Elem
import scala.xml.NodeSeq


object XMLObjectsManager {
   
   def getCleanXMLParagraph(lineSeparator: String): XMLParagraph = new XMLParagraph("", new XMLPosition(0, 0, 0, 0), new XMLParagraphOptionsContainer(XMLParagraphOptions.NONE), List(), lineSeparator, "", "")
  
   // Since the xml file sometimes creates different fonts with the same features (size, family, etc), it
   // is important to create a structure that takes into account this detail and hides it.
   // The XMLFont class performs this job. XMLFontsContainer just contains the list of XMLFont objects
   private def getFontsContainer(xml: Elem): Option[XMLFontsContainer] = {
       val pages = (xml \\ "page")
       
       
       // This method updates the previous XMLFont list with the new fonts defined in the page.
	   def getXMLFontListFromPage(previousList: List[XMLFont], pageNumber: String): List[XMLFont] = {
		   // Recursive method that runs through the page's fonts list and updates the given XMLFont list
    	   def constructFontLists(fonts: NodeSeq, accu: List[XMLFont]): List[XMLFont] = {
		     	       
    	       // This method runs through the given XMLFont list and checks if the new XMLFont defined object is already in the list
		       def checkIfExist(xmlFont: XMLFont, list: List[XMLFont], accu: List[XMLFont]): List[XMLFont] = list match{
		         case List() => (xmlFont :: accu).reverse // Putting the new XMLFont at the end of the list
		         case x::xs => if(x.compareXMLFont(xmlFont)) (x.addID(xmlFont.getID) :: accu).reverse ::: xs // Updating the given XMLFont list
		         			   else checkIfExist(xmlFont, xs, x :: accu)
		       }
		       
		       fonts.isEmpty match {
		           case true => accu
		           case false => {
		             // Creating then new XMLFont object according to the font parameters
		             val xmlFont = new XMLFont((fonts.head \ "@id").text, (fonts.head \ "@size").text, (fonts.head \ "@family").text, (fonts.head \ "@color").text)
		             constructFontLists(fonts.tail, checkIfExist(xmlFont, accu, List()))
		           }
		       }
			     
		   }
		   
		   // Getting the page
		   val page = pages filter((n) => (n \ "@number").text.equals(pageNumber))
		   
		   if(page.length != 1) previousList
		   else {
		     val fonts = page.head \\ "fontspec"
		
		     // Updating the XMLFont list
		     constructFontLists(fonts, previousList)
		   }
	   }
	   
       // Method for running through the pages list
	   def constructUntilPage(pagesNumber: Int, accu: List[XMLFont], currentPage: Int): List[XMLFont] = {
		   if(currentPage > pagesNumber) accu
		   else constructUntilPage(pagesNumber, getXMLFontListFromPage(accu, currentPage.toString()), currentPage + 1)
	   } 

	   // Getting the xml fonts list
	   val xmlFonts = constructUntilPage(pages.length, List(), 1)
	   
	   // Creating the container
	   if(xmlFonts.length >= 1) Some(new XMLFontsContainer(xmlFonts))
	   else None
   }
   
   // This method creates the pages of the document. The final list is reversed
   private def constructXMLPages(pages: NodeSeq, fontsContainer: Option[XMLFontsContainer], lineSeparator: String): Option[List[XMLPage]] = {
	 // This method constructs the xml page
     def constructXMLPage(page: xml.Node): Option[XMLPage] = {
	     try {
	         // First we get the global page parameters
		     val number = (page \ "@number").text.toInt
		     val x = (page \ "@top").text.toInt
		     val y = (page \ "@left").text.toInt
		     val width = (page \ "@width").text.toInt
		     val height = (page \ "@height").text.toInt
		     val position = new XMLPosition(x, y, width, height)
		     // Construction of the paragraphs
		     val paragraphs = XMLParagraphsConstructor.constructXMLParagraphs(page, position, fontsContainer.get, lineSeparator)
		      
		     if (paragraphs != None) Some(new XMLPage(number, position, paragraphs.get))
		     else None
	     }catch {
	         case _ => None
	     }
      
	 }
     
	 // Recursive method in order to run through the pages list 
     def constructXMLPages0(pages: NodeSeq, accu: Option[List[XMLPage]]): Option[List[XMLPage]] = {            
        if(pages.isEmpty) accu
        else {
           val page = constructXMLPage(pages.head)
           
           if(page == None) None
           else constructXMLPages0(pages.tail, Some(page.get :: accu.get))
        }
     }
     
     if(fontsContainer != None) constructXMLPages0(pages, Some(List()))
     else None
   }
   
   // This method constructs the xml document of the file
   def constructXMLDocument(xml:Elem, lineSeparator:String): Option[XMLDocument] = {     
      // getting fontsContainer and xml pages
      val fontsContainer = getFontsContainer(xml)
      val xmlPages = constructXMLPages(xml \\ "page", fontsContainer, lineSeparator)
      
      // If everything is fine, then create the document
      if(xmlPages != None) Some(new XMLDocument(fontsContainer.get, xmlPages.get.reverse))
      else None
   }
}