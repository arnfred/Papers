package paper

object ExtractionRegexes {
	val and = """(and|&)"""
	val quoteB = """(“|\")"""
	val quoteE = """(”|\")"""
	val tabulation = """ \t """
	val authorsSeparator = """( ?, and | ?, | and | ?, \t | \t | & )"""
	val referencesName = """(REFERENCES|R EFERENCES|References|R eferences)"""
	val titleTermination = """[\.\?]"""
}

// This class is a the top of the hierarchy
abstract class ReferenceProcessor {
	def extract: (Option[Title], Option[List[Author]])
}

// This class recognizes the format of a particular reference and creates a suitable object ready to extract relevant information
object ReferenceProcessorDispatcher {
	def getReferenceProcessor(ref: String): ReferenceProcessor = {
		if(("""^\[\d+\].+""" + ExtractionRegexes.quoteB + """.+""" + ExtractionRegexes.quoteE + """(.+)?$""").r.findFirstIn(ref).isDefined) new NumberedReferenceProcessor1(ref)
		else if(("""^\[\d+\].+$""").r.findFirstIn(ref).isDefined) new NumberedReferenceProcessor2(ref)
		else if(("""^.+?\(\d+\)\..+?\..+?\.$""").r.findFirstIn(ref).isDefined) new TextualReferenceProcessor1(ref)
		else new UnsupportedReferenceProcessor(ref)
	}
}

// This is the default class called when the dispatcher can't recognize a particular reference format
case class UnsupportedReferenceProcessor(ref: String) extends ReferenceProcessor {
	def extract: (Option[Title], Option[List[Author]]) = (None, None)
}




// This class can extract information out of a reference having the following format: [digit] authors "title"
case class NumberedReferenceProcessor1(ref: String) extends ReferenceProcessor {
	def extract: (Option[Title], Option[List[Author]]) = {
		def extractTitle: Option[Title] = {
		  	val t = (ExtractionRegexes.quoteB + """.+""" + ExtractionRegexes.quoteE).r.findFirstIn(ref)
		  	if(!t.isDefined) return None
		  	  
		  	// Dropping the " characters
			val title = t.get.drop(1).dropRight(1)
			val finalTitle = if(title.last.equals(',')) title.dropRight(1) else title
			Some(new Title(finalTitle))
		}
		
		def extractAuthors: Option[List[Author]] = {
			val t = ("""^\[\d+\].+""" + ExtractionRegexes.quoteB).r.findFirstIn(ref)
			if(!t.isDefined) return None
			
			val totAuths = t.get
			val auths = ("""^\[\d+\]""").r.replaceAllIn(totAuths.dropRight(1), "")

			val authorsStringList = ("""([A-Z]\.[ -])+.+?( """ + ExtractionRegexes.and + """|,)""").r.findAllIn(auths).toList
		  
			Some(authorsStringList.map((s:String) => new Author(ExtractionRegexes.and.r.replaceAllIn(s, "").replace(",", ""))))
		}
		
		(extractTitle, extractAuthors)
	}
}


// This class can extract information out of a reference having the following format: [digit] authors. title
case class NumberedReferenceProcessor2(ref: String) extends ReferenceProcessor {
	def extract: (Option[Title], Option[List[Author]]) = {
		val authRegex = ("""([A-Z]\.[ -])+.+?( """ + ExtractionRegexes.and + """|,|, """ + ExtractionRegexes.and + """|\.)""").r
	  	
		val ref2 = ("""^\[\d+\]( )""").r.replaceAllIn(ref, "")
		val authorsStringList = authRegex.findAllIn(ref2).toList
		
		val auths = Some(authorsStringList.map((s:String) => new Author(ExtractionRegexes.and.r.replaceAllIn(s, "").replace(",", "").dropRight(1))))
		
		val t = ("""^.+?""" + ExtractionRegexes.titleTermination + """""").r.findFirstIn(authRegex.replaceAllIn(ref2, ""))
		if(!t.isDefined) return (None, None)
		
		// Dropping until a the capital character is founded
		val title = Some(new Title(t.get.replace(".", "").dropWhile((c:Char) => """[^A-Z]""".r.findFirstIn(""+c).isDefined)))
		
		(title, auths)
	}
}

// This class can extract information out of a reference having the following format: authors (digits) title
case class TextualReferenceProcessor1(ref: String) extends ReferenceProcessor {
	def extract: (Option[Title], Option[List[Author]]) = {
		def extractTitle: Option[Title] = {
		  	val t = ("""\)\..+?""" + ExtractionRegexes.titleTermination + """""").r.findFirstIn(ref)
		  	if(!t.isDefined) return None
		  	
			Some(new Title(t.get.drop(3)))
		}
		
		def extractAuthors: Option[List[Author]] = {
		  	val t = """^.+?\(""".r.findFirstIn(ref)
		  	if(!t.isDefined) return None
		  	
		  	// Dropping the " (" string
			val auths = t.get.dropRight(2)

			val authorsStringList = (""".+?,( [A-Z]\.)+(, (""" + ExtractionRegexes.and + """ )?)?""").r.findAllIn(auths).toList
		  
			Some(authorsStringList.map((s:String) => new Author((""" """ + ExtractionRegexes.and + """ """).r.replaceAllIn(s, "").replace(",", ""))))
		}
		
		(extractTitle, extractAuthors)
	}
}
