package paper

object Analyze {
  def main(args : Array[String]): Unit= {
    // create analyzer
    val A : Analyzer = new Analyzer()

    // Check that a directory is supplied (there is an argument)
    if (args.length == 0 || args.length > 2) {
      println("You should provide at least a path and at most a path and an option. Type -h for help.");
    }

    else {
      val options = readOptions(args.last)

      // Then go ahead
      A.analyze(args(0), options)
    }
  }

  // Reads in the options and converts them to a map of options
  def readOptions(s : String) : Map[String,Boolean] = {

    var options : Map[String,Boolean] = Map();

    if (s.length > 0 && s(0) == '-') {
      // Define options as always false, except
      options = Map().withDefaultValue(false)

      // Check for parsing
      if (s.contains('p')) options += ("parse" -> true)

      // Check for getting schedule
      if (s.contains('s')) options += ("xmlschedule" -> true)

      // Check for extending
      if (s.contains('e')) options += ("extend" -> true)

      // Check for linking
      if (s.contains('l')) options += ("link" -> true)

      // Check for linking
      if (s.contains('g')) options += ("graph" -> true)

      if(s.contains('h')) { 
        println("""How to call: Analyze [path] 
        [parameter]?\nPARAMETERS:\n\t-p : parsing\n\t-s : looks for xml 
        scheduler\n\t-c : compare\n\t-l : link\n\t-g : create graph\n\t-h 
        : shows this help page\n\tnothing : do everything"""); 
      }

    }

    else {
      // If no options are supplied we do everything by default
      options = Map().withDefaultValue(true)
    }

    return options
  }
}



class Analyzer extends Object with LoadPaper
                              with ParsePaper 
                              with ExtendPaper
                              with BagOfWordsLSI
                              with XMLScheduleParser
                              with Graphs {

  // Set a limit in percent for when papers get an edge between them
  val limit : Int = 1

  // Get cached papers in this order
  val cache : List[String] = List(Cache.linked, Cache.extended, Cache.scheduled, Cache.parsed)

  // Set sources we want to extend with
  //val sources : List[PaperSource] = List(TalkDates, TalkRooms, PdfLink)
  val sources : List[PaperSource] = List(PdfLink)

  // Analyze a paper
  def analyze(paperPos: String, options: Map[String, Boolean]): List[Paper] = {

    var papers : List[Paper] = List();

    // Get a list of parsed papers
    if (options("parse") == true) {
      papers = loadAndParse(paperPos, cache, XMLParser, XMLConverterLoader)
    }

    // Mix in the schedule XML data
    if (options("xmlschedule") == true) {
      papers = getXMLSchedule(paperPos, papers)
    }

    // Extend papers with tertiary data
    if (options("extend") == true) {
      papers = extend(paperPos, papers, sources)
    }

    // Compare the papers individually
    if (options("link") == true) {
      papers = compareBoWLSI(paperPos, papers, limit)
    }

    // Create graph
    if (options("graph") == true) {
      val graph : Graph = getGraph(paperPos, papers)

      // Print graph to file 'data.json'
      graph.save
    }

    // Now return the papers as is
    return papers
  }
}
