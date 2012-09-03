package paper

/** Abstract Syntax Trees for terms. */
sealed abstract class Term


// Try to keep this immutable
case class Paper(val id :       Int, 
                 val index :    Int,
                 val title:     Title, 
                 val authors:   List[Author], 
                 val abstr:     Abstract, 
                 val body:      Body, 
                 val refs:      List[Reference], 
                 val meta:      Map[String, String],
                 val links :    List[Link]) extends Term {

                 // Add a field that contains options, such as parsed and linked

  val parsed : Boolean = false
  val linked : Boolean = false

  override def toString: String = title + "\n" + authors.mkString(", ") + "\n" + abstr + "\n" + body  + "\n" + refs.mkString("\n")

  def getDistinctNames : List[String] = {
    val as = authors ::: refs.flatMap(r => r.authors)
    val names = as.map(a => a.toString)
    return names.distinct
  }

  def clean : Paper =
    return Paper(id, index, title, authors.filter(a => a.name.length > 4), abstr, body, refs.map(r => r.clean), meta, links)

  def setMeta(p : (String, String)) : Paper = 
    return Paper(id, index, title, authors, abstr, body, refs, meta + p, links)

  def setTitle(t : Title) : Paper =
    return Paper(id, index, t, authors, abstr, body, refs, meta, links)

  def setAuthors(as : List[Author]) : Paper =
    return Paper(id, index, title, as, abstr, body, refs, meta, links)

  def hasMeta(l : String) : Boolean = (meta.get(l) != None)

  def setId(newId : Int) : Paper = 
    return Paper(newId, index, title, authors, abstr, body, refs, meta, links)

  def setIndex(newIndex : Int) : Paper = 
    return Paper(id, newIndex, title, authors, abstr, body, refs, meta, links)

  def setLinks(newLinks : List[Link]) : Paper = 
    return Paper(id, index, title, authors, abstr, body, refs, meta, newLinks)
    
  def setAbstract(newAbstract : Abstract) : Paper = 
    return Paper(id, index, title, authors, newAbstract, body, refs, meta, links)
    
  def setBody(newBody : Body) : Paper = 
    return Paper(id, index, title, authors, abstr, newBody, refs, meta, links)
    
  def setReferences(newRefs : List[Reference]) : Paper = 
    return Paper(id, index, title, authors, abstr, body, newRefs, meta, links)
}

case class Title(t: String) extends Term {
  override def toString: String = t
}

case class Author(name: String) extends Term {
  override def toString: String = name
}

case class Abstract(text: String) extends Term {
  override def toString : String = "Abstract:\t" + text.take(40) + " ... "
}

case class Body(text: String) extends Term {
  override def toString: String = "Body:\t\t" + text.take(100) ++ " ... \n"
}

case class Reference(authors: List[Author], title: Title) extends Term {
  def clean : Reference = return Reference(authors.filter(a => a.name.stripMargin.length > 0), title)
  override def toString : String = authors.mkString("\n") + "\n--\n" + title
}

case class Link(index : Int, weight : Int) extends Term {
  override def toString : String = index + " " + weight
}

