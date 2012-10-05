package paper
import java.io.File

trait XMLScheduleParser {

  import scala.xml._
  import scala.collection.immutable.Map._

  // Overall function that loads the xml schedule and returns the papers with the extra data
  def getXMLSchedule(paperPos : String, papers : List[Paper]) : List[Paper] = {
	val path = paperPos + "/schedule.xml"
	
    if (!(new File(path)).exists()) return papers;
	
    // Parse schedule
    val xml : Map[Int, Elem] = parse(path)

    // match schedule with papers
    return matchXML(xml, papers);
  }

  // Function for taking care of parsing the xml
  def parse(paperPos : String) : Map[Int, Elem] = {

    // Load schedule file
    val schedule : Elem = XML.loadFile(paperPos)

    // Initialize Map
    var data : Map[Int, Elem] = Map.empty

    // For each session record room, date, session
    (schedule \\ "session") foreach (session => {

      var date = (session \ "date").text
      var room = (session \ "room").text
      var sess = (session \ "code").text + ": " + (session \ "sessiontitle").text

      // Now for each paper record starttime, endtime, paperid, papertitle, abstract
      (session \\ "paper") foreach (paper => {
        // Create a hunk of xml containing all the data
        var xml = <session><date>{ date }</date><room>{ room }</room><sess>{ sess }</sess><info>{ paper }</info></session>
        var id = (paper \\ "paperid").text
        data = data + (id.toInt -> xml)
      })
    })

    // Return the map of all the data
    return data
  }

  // Function for putting the xml in the right paper
  def matchXML(xml : Map[Int, Elem], papers : List[Paper]) : List[Paper]= {
    // Loop through all papers and add the xml elements the appropriate one
    return for (p <- papers) yield xml.get(p.id) match {
      case None             => println("No schedule data for paper with id: " + p.id); p
      case Some(data)       => {
        // Get resulting paper
        val result = setXMLData(data, p)

        // Save result
        Cache.save(result, Cache.scheduled)

        // Return result
        result
      }
    }
  }

  // Putting the xml in a paper
  def setXMLData(xml : Elem, paper : Paper) : Paper = {
    paper.setMeta("xmldate"        -> getDate(xml))
         .setMeta("xmlroom"        -> getRoom(xml \\ "room"))
         .setMeta("xmlsession"     -> (xml \\ "sess").text)
         .setMeta("xmlstarttime"   -> (xml \\ "starttime").text)
         .setMeta("xmlendtime"     -> (xml \\ "endtime").text)
         .setMeta("xmlpaperid"     -> (xml \\ "paperid").text)
         .setMeta("xmlsessionid"   -> (xml \\ "sessionid").text)
         .setMeta("xmlpapertitle"  -> (xml \\ "papertitle").text)
         .setMeta("xmlabstract"    -> (xml \\ "abstract").text)
         .setMeta("xmlauthors"     -> getAuthors(xml \\ "authors").mkString(", "))
         .setTitle(new Title((xml \\ "papertitle").text))
         .setAuthors(getAuthors(xml \\ "authors").map(a => Author(formatAuthors(a))))
  }

  // Converts an authors XML note to string
  def getAuthors(authors : NodeSeq) : List[String] = {
    return (for (a <- (authors \ "author")) yield (a \\ "name").text).toList
  }

  def getRoom(room : NodeSeq) : String = {
    room.text match {
      case "Track 1"              => "Kresge Rehearsal B (030)"
      case "Track 2"              => "Kresge Auditorium (109)"
      case "Track 3"              => "Stratton S. de P. Rico (202)"
      case "Track 4"              => "Stratton 20 Chimneys (306)"
      case "Track 5"              => "Kresge Little Theatre (035)"
      case "Track 6"              => "Kresge Rehearsal A (033)"
      case "Track 7"              => "Stratton (407)"
      case "Track 8"              => "Stratton (491)"
      case "Track 9"              => "Stratton West Lounge (201)"
    }
  }

  def getDate(xml : Elem) : String = {
    import java.util.Calendar
    import java.sql.Timestamp

    var date = (xml \\ "date").text
    var time = (xml \\ "starttime").text

    var dateNum : Int = date.takeWhile(_.isDigit).toInt
    var hourNum : Int = time.split(':').head.toInt
    var minNum : Int = time.split(':').last.toInt

    // Get Calendar
    var c = Calendar.getInstance

    // Set starting point
    c.set(2012, 6, dateNum, hourNum, minNum)
    c.set(Calendar.SECOND,0)
    c.set(Calendar.MILLISECOND,0)

    // Get a timeStamp
    var t = (new Timestamp(c.getTime.getTime).getTime).toString

    return t
  }

  def formatAuthors(name : String) : String = {
    var result = ""
    var names = name.split(" ").filter(n => n.length > 0)
    if (names.length > 0) {
      result = names.init.filter(n => n.length > 0).map(n => n.head).mkString("",". ",". ") + names.last
    }
    return result
  }

}
