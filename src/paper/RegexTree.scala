package paper

sealed abstract class RegexTree

case class RegexNode(elems: List[RegexTree], id: Int) extends RegexTree
case class RegexLeaf(regex: String) extends RegexTree

object RegexManager {
	def getRegex(regex: RegexTree) : String = regex match {
	  
	  case RegexNode(e, id) => {
	    def getRegexString(e: List[RegexTree], accu: String): String = e match {
	      case List() => accu
	      case x::xs => getRegexString(xs, accu + getRegex(x))
	    }
	    
	    """(""" + getRegexString(e, "") + """)"""
	  }
	  
	  case RegexLeaf(r) => r
	}
}