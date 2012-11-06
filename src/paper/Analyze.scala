package paper

object Analyze {
  def main(args : Array[String]): Unit= {
    // create analyzer
    val A : Analyzer = new Analyzer()    
    
    // Check that a directory is supplied (there is an argument)
    if (args.length == 0 || args.length > 2) {println("You should provide at leath a path and at most a path and an option. Type -h for help.");List()}
    
    else if(args.contains("-h")) { println("How to call: Analyze [path] [parameter]?\nPARAMETERS:\n\t-p : parsing\n\t-s : looks for xml scheduler\n\t-c : compare\n\t-e : extend\n\t-g : create graph\n\t-h : shows this help page\n\tnothing : do everything"); List()}
    // Then go ahead
    else A.analyze(args(0), args.toList.tail)
  }
}

class Analyzer extends Object with LoadPaper
                              with ParsePaper 
                              with ExtendPaper
                              with bagOfWords
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
  def analyze(paperPos: String, options: List[String]): List[Paper] = {
    // Get a list of parsed papers
    val papers : Option[List[Paper]] = if(options.isEmpty || options.contains("-p")) Some(loadAndParse(paperPos, cache, XMLParser, XMLConverterLoader)) else None

    // Mix in the schedule XML data
    val xmlPapers : Option[List[Paper]] = if(options.isEmpty || options.contains("-s")) Some(getXMLSchedule(paperPos, papers)) else None

    // Extend papers with tertiary data
    val extendedPapers : Option[List[Paper]] = if(options.isEmpty || options.contains("-e")) Some(extend(paperPos, xmlPapers, sources)) else None
    
    // Compare the papers individually
    val comparedPapers : Option[List[Paper]] = if(options.isEmpty || options.contains("-c")) Some(compareBoW(paperPos, extendedPapers, limit)) else None


    if(options.isEmpty || options.contains("-g")){
	    // Create graph
	    val graph : Graph = getGraph(paperPos, comparedPapers)
	
	    // Print graph to file 'data.json'
	    graph.save
  	}
	
	if(options.contains("-p")) return papers.get
	else if(options.contains("-s")) return xmlPapers.get
	else if(options.contains("-e")) return extendedPapers.get
	else if(options.isEmpty || options.contains("-c")) return comparedPapers.get
	
	List()
  }
}
