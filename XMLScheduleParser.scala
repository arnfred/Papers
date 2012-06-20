package paper

trait XMLScheduleParser {

  import scala.xml._
  import scala.collection.immutable.Map._

  // Overall function that loads the xml schedule and returns the papers with the extra data
  def getXMLSchedule(paperPos : String, papers : List[Papers]) : List[Papers] = {

    // Parse schedule
    val xml : Map[Int, Elem] = parse(paperPos)

    // match scedule with papers
    val papers : List[Papers] = matchXML(xml, papers);
  }

  // Function for taking care of parsing the xml
  def parse(paperPos : String) : Map[Int, Elem] = {

    // Load schedule file
    val schedule : Elem = XML.loadFile(PaperPos + "/schedule.xml")

    // Initialize Map
    var data : Map[String,Elem] = Map.empty

    // For each session record room, date, session
    (schedule \\ "session") foreach (session => {

      var date = (session \ "date").text
      var room = (session \ "room").text
      var sess = (session \ "code").text + ": " + (session \ "sessiontitle").text

      // Now for each paper record starttime, endtime, paperid, papertitle, abstract
      (session \\ "papers") foreach (paper => {
        // Create a hunk of xml containing all the data
        var xml = <session><date>{ date }</date><room>{ room }</room><sess>{ sess }</sess><info>{ paper \ "paper" }</info></session>
        data = data + ((paper \\ "paperid").text.toInt -> xml)
      })
    })

    // Return the map of all the data
    return data
  }

  // Function for putting the xml in the right paper
  def matchXML(xml : Map[String, Elem], papers : List[Paper]) : List[Paper]= {
    // Loop through all papers and add the xml elements the appropriate one
    papers = for (p <- papers) yield xml.get(p.id) match {
      case None             => println("No schedule data for paper with id: " + p.id)); p
      case Some(data)       => setXMLData(data, p)
    }
  }

  // Putting the xml in a paper
  def setXMLData(xml : Elem, paper : Paper) : Paper = {
    paper.setMeta(("xmldate"        -> (xml \\ "date").text))
         .setMeta(("xmlroom"        -> (xml \\ "room").text))
         .setMeta(("xmlsession"     -> (xml \\ "sess").text)) 
         .setMeta(("xmlstarttime"   -> (xml \\ "starttime").text))
         .setMeta(("xmlendtime"     -> (xml \\ "endtime").text)) 
         .setMeta(("xmlpaperid"     -> (xml \\ "paperid").text)) 
         .setMeta(("xmlsessionid"   -> (xml \\ "sessionid").text)) 
         .setMeta(("xmlpapertitle"  -> (xml \\ "papertitle").text)) 
         .setMeta(("xmlabstract"    -> (xml \\ "abstract").text)) 
         .setMeta(("xmlauthors"     -> getAuthors(xml \\ "authors"))) 
  }

  // Converts an authors XML note to string
  def getAuthors(authors : Elem) : String = {
    as : String = (for (a <- (authors / "author")) yield a.text).mkString(", ")
  }

}
