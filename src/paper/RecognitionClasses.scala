package paper

object ExtractionRegexes {
	val and = """(and|&)"""
	val quoteB = """(“|\")"""
	val quoteE = """(”|\")"""
	val tabulation = """ \t """
	val authorsSeparator = """( ?, and | ?, | and | ?, \t | \t | & )"""
	val referencesName = """(REFERENCES|R EFERENCES|References|R eferences)"""
}

// This class is a the top of the hierarchy
abstract class ReferenceProcessor {
	def extract: (Option[Title], Option[List[Author]])
}

// This class recognizes the format of a particular reference and creates a suitable object ready to extract relevant information
object ReferenceProcessorDispatcher {
	def getReferenceProcessor(ref: String): ReferenceProcessor = {
		if(("""^\[\d+\].+""" + ExtractionRegexes.quoteB + """.+""" + ExtractionRegexes.quoteE + """(.+)?$""").r.findFirstIn(ref).isDefined) new NumberedReferenceProcessor1(ref)
		//else if(("""^\[\d+\].+$""").r.findFirstIn(ref).isDefined) new NumberedReferenceProcessor2(ref)
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
			val title = (ExtractionRegexes.quoteB + """.+""" + ExtractionRegexes.quoteE).r.findFirstIn(ref).get.drop(1).dropRight(1)
			val finalTitle = if(title.last.equals(',')) title.dropRight(1) else title
			Some(new Title(finalTitle))
		}
		
		def extractAuthors: Option[List[Author]] = {
			val totAuths = ("""^\[\d+\].+""" + ExtractionRegexes.quoteB).r.findFirstIn(ref).get
			val auths = ("""^\[\d+\]""").r.replaceAllIn(totAuths.dropRight(1), "")

			val authorsStringList = ("""([A-Z]\.[ -])+.+?( """ + ExtractionRegexes.and + """|,)""").r.findAllIn(auths).toList
		  
			Some(authorsStringList.map((s:String) => new Author(ExtractionRegexes.and.r.replaceAllIn(s, "").replace(",", ""))))
		}
		
		(extractTitle, extractAuthors)
	}
}

case class NumberedReferenceProcessor2(ref: String) extends ReferenceProcessor {
	def extract: (Option[Title], Option[List[Author]]) = {
		//val auths = """^.+?\(""".r.findFirstIn(ref).get.dropRight(2)

		  //val authorsStringList = (""".+?,( [A-Z]\.)+(, (""" + ExtractionRegexes.and + """ )?)?""").r.findAllIn(auths).toList
		  
		  //Some(authorsStringList.map((s:String) => new Author((""" """ + ExtractionRegexes.and + """ """).r.replaceAllIn(s, "").replace(",", ""))))

		// to do
		(None, None)
	}
}

// This class can extract information out of a reference having the following format: authors (digits) title
case class TextualReferenceProcessor1(ref: String) extends ReferenceProcessor {
	def extract: (Option[Title], Option[List[Author]]) = {
		def extractTitle: Option[Title] = Some(new Title("""\)\..+?[\.\?]""".r.findFirstIn(ref).get.drop(3)))
		
		def extractAuthors: Option[List[Author]] = {
			val auths = """^.+?\(""".r.findFirstIn(ref).get.dropRight(2)

			val authorsStringList = (""".+?,( [A-Z]\.)+(, (""" + ExtractionRegexes.and + """ )?)?""").r.findAllIn(auths).toList
		  
			Some(authorsStringList.map((s:String) => new Author((""" """ + ExtractionRegexes.and + """ """).r.replaceAllIn(s, "").replace(",", ""))))
		}
		
		(extractTitle, extractAuthors)
	}
}
