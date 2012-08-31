package paper

object ExtractionRegexes {
	val and = """(and|&)"""
	val quoteB = """(“|\")"""
	val quoteE = """(”|\")"""
	val tabulation = """ \t """
	val authorsSeparator = """( ?, and | ?, | and | ?, \t | \t | & )"""
	val referencesName = """(REFERENCES|R EFERENCES|References|R eferences)"""
}

abstract class ReferenceExtractor {
	def extractTitle(): Option[Title] = this match {
	  case NumberedReferenceExtractor1(ref) => {
		  val title = (ExtractionRegexes.quoteB + """.+""" + ExtractionRegexes.quoteE).r.findFirstIn(ref).get.drop(1).dropRight(1)
		  val finalTitle = if(title.last.equals(',')) title.dropRight(1) else title
		  Some(new Title(finalTitle))
	  }
	  case TextualReferenceExtractor1(ref) => Some(new Title("""\)\..+?[\.\?]""".r.findFirstIn(ref).get.drop(3)))
	  case UnsupportedReferenceExtractor(ref) => None
	}
	
	
	def extractAuthors(): Option[List[Author]] = this match {
	  case NumberedReferenceExtractor1(ref) => {
		  val totAuths = ("""^\[\d+\].+""" + ExtractionRegexes.quoteB).r.findFirstIn(ref).get
		  val auths = ("""^\[\d+\]""").r.replaceAllIn(totAuths.dropRight(1), "")

		  val authorsStringList = ("""([A-Z]\.[ -])+.+?( """ + ExtractionRegexes.and + """|,)""").r.findAllIn(auths).toList
		  
		  Some(authorsStringList.map((s:String) => new Author((""" """" + ExtractionRegexes.and).r.replaceAllIn(s, "").replace(",", ""))))
	  }
	  case TextualReferenceExtractor1(ref) => {
		  val auths = """^.+?\(""".r.findFirstIn(ref).get.dropRight(2)

		  val authorsStringList = (""".+?,( [A-Z]\.)+(, (""" + ExtractionRegexes.and + """ )?)?""").r.findAllIn(auths).toList
		  
		  Some(authorsStringList.map((s:String) => new Author((""" """ + ExtractionRegexes.and + """ """).r.replaceAllIn(s, "").replace(",", ""))))
	  }
	  case UnsupportedReferenceExtractor(ref) => None
	}
}

case class NumberedReferenceExtractor1(ref: String) extends ReferenceExtractor
case class TextualReferenceExtractor1(ref: String) extends ReferenceExtractor
case class UnsupportedReferenceExtractor(ref: String) extends ReferenceExtractor

object ReferenceExtractorDispatcher {
	def getReferenceExtractor(ref: String): ReferenceExtractor = {
		if(("""^\[\d+\].+""" + ExtractionRegexes.quoteB + """.+""" + ExtractionRegexes.quoteE + """(.+)?$""").r.findFirstIn(ref).isDefined) new NumberedReferenceExtractor1(ref)
		else if(("""^.+?\(\d+\)\..+?\..+?\.$""").r.findFirstIn(ref).isDefined) new TextualReferenceExtractor1(ref)
		else new UnsupportedReferenceExtractor(ref)
	}
}


object AuthorsDetector {
  
	def detect(paragraphs: List[XMLParagraph]): Option[List[Author]] = {
		val minTop = paragraphs.map((p:XMLParagraph) => p.getPosition.getY).min
		val unprocessedAuthorsList = paragraphs.filter((p:XMLParagraph) => p.getPosition.getY == minTop)
		
		val authors = unprocessedAuthorsList.flatMap((p:XMLParagraph) => ("""(.+?""" + ExtractionRegexes.authorsSeparator + """)|(.+?$)""").r.findAllIn(""" [0-9]+""".r.replaceAllIn(p.getLines.head.getText, "")))
		Some(authors.map((s: String) => new Author(ExtractionRegexes.authorsSeparator.r.replaceAllIn(s, ""))))
	}
}


