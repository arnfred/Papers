package paper

/** Abstract Syntax Trees for terms. */
abstract class Term

case class Paper(title: Title, authors: Authors) extends Term

case class Title(t: String) extends Term {
  override def toString: String = "Title: " + t
}

case class Authors(as: List[Author]) extends Term {
  override def toString: String = {
    if (as.length == 1) "Author: " + as.head.toString
    else "Authors:\n" + as.mkString("\n  ")
  }
}

case class Author(name: String, school: String) extends Term {
  override def toString: String =  name + ", " + school
}

