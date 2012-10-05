package paper

import scala.util.parsing.input._
import scala.collection.immutable.Stream
import scala.io.Source
import java.io._


object Cache {
  
  // Constants
  val dir = "cache/"
  val parsed = "parsed"
  val extended = "extended"
  val linked = "linked"
  val scheduled = "scheduled"
  val bad = "bad"

  import scala.io.Source

  def bad(file : File) : Unit = {
    val f = new File(dir + file.getName + "." + bad)
    // Make sure file exists
    if(!f.exists) f.createNewFile
  }

  def save(p : Paper, postfix : String) : Unit = {
    val orig = new File(p.meta("file"))
    val f = new File(dir + orig.getName + "." + postfix)
    // Make sure file exists
    if(!f.exists) f.createNewFile
    val w = new PrintWriter(f)

    // Print paper
    w.println("[[[ ID ]]]" + "\n" + p.id)
    w.println("[[[ INDEX ]]]" + "\n" + p.index)
    w.println("[[[ TITLE ]]]" + "\n" + p.title)
    w.println("[[[ AUTHORS ]]]" + "\n" + p.authors.mkString("\n"))
    w.println("[[[ ABSTR ]]]" + "\n" + p.abstr.text)
    w.println("[[[ BODY ]]]" + "\n" + p.body.text)
    w.println("[[[ REFS ]]]" + "\n" + p.refs.mkString("\n----\n"))
    w.println("[[[ META ]]]" + "\n" + p.meta.mkString("\n"))
    w.println("[[[ LINKS ]]]" + "\n" + p.links.mkString("\n----\n"))
    w.close
  }

  def load(file : File) : Paper = {

    // Printout
    println("loading file " + file.getName + " from cache")

    // Get file and read in lines
    val lines : Iterator[String] = Source.fromFile(file).getLines
    
    // Variables
    var vars : Map[String, List[String]] = Map.empty.withDefaultValue(Nil)
    var current = "unknown";
    
    // Order the information
    for (l <- lines) l match {
      case "[[[ ID ]]]"         => current = "id"
      case "[[[ INDEX ]]]"      => current = "index"
      case "[[[ TITLE ]]]"      => current = "title"
      case "[[[ AUTHORS ]]]"    => current = "authors"
      case "[[[ ABSTR ]]]"      => current = "abstr"
      case "[[[ BODY ]]]"       => current = "body"
      case "[[[ REFS ]]]"       => current = "refs"
      case "[[[ META ]]]"       => current = "meta"
      case "[[[ LINKS ]]]"      => current = "links"
      case line                 => vars = vars + (current -> (vars(current) ::: List(line)))
    }

    return Paper(vars("id").head.toInt, 
                 vars("index").head.toInt,
                 Title(vars("title").head), 
                 stringToAuthors(vars("authors")),
                 Abstract(vars("abstr").mkString("\n")),
                 Body(vars("body").mkString("\n")),
                 stringToRefs(vars("refs")),
                 setMeta(vars("meta")),
                 stringToLinks(vars("links")))

  }

  def setMeta(s : List[String]) : Map[String, String] = {
    var m : Map[String, String] = Map.empty
    // Looping through all the maps
    for (e <- s) {
      m = m + Pair((e.split(" -> "))(0), (e.split(" -> "))(1))
    }
    return m
  }

  def stringToLinks(s : List[String]) : List[Link] = {
    return s.filter(l => l.split(" ").length == 2).map(l => Link(l.split(" ").head.toInt, l.split(" ").last.toInt))
  }

  def stringToAuthors(s : List[String]) : List[Author] = {
    return s.map(l => Author(l))
  }

  def stringToRefs(s : List[String]) : List[Reference] = {
    // Variables
    var map : Map[String, List[String]] = Map.empty.withDefaultValue(Nil)
    var index : Int = 0
    var current = "authors" + index

    // Make sure we have any references
    if (s.head == "") return Nil
    
    // Order the information
    for (l <- s) l match {
      case "--"         => current = "title" + index
      case "----"       => index += 1; current = "authors" + index
      case line         => map = map + (current -> (line :: map(current)))
    }

    // Now gather it
    val refs = for (i <- 0 to index) yield Reference(stringToAuthors(map("authors" + i)), Title(map("title" + i).head))
    return refs.toList
  }
}



trait LoadPaper {

  def load(name : String, postfix : List[String], parser : Parsers, loader : FileLoader) : List[Paper] = {
	// Get file handle of original file or directory
    val orig = new File(name)

    // Check that directory or file exists
    if (!orig.exists) sys.error("Something is wrong with the file or directory in the argument")

    // If exists, set name and file
    // In case it's a directory, let the file array contain all the files of the directory (regex utilization)
    val files : List[File]  = if(orig.isDirectory) SystemHelper.getFilesFromDirectory(orig) else List(orig)
    val fnames : List[String]  = if(orig.isDirectory) files.map(f => name ++ f.getName) else List(name)


    // If postfix exists, try loading from cache
    val somePapers : List[Option[Paper]] = if (postfix != Nil) files.map(f => loadFromCache(f, postfix)) else Nil

    // All papers that weren't loaded by cache are loaded by file
    val finalPapers = somePapers.zip(files).map(p => if (p._1 == None) loadFromFile(p._2, parser, loader) else p._1)

    // Filter papers for None's and set index
    val papers : List[Paper] = finalPapers.filter(p => p != None).zipWithIndex.map({case Pair(p,i) => p.get.setIndex(i) }).toList

    return papers
  }


  // Loads a paper from a text file and parses it. It has been modified in order to make loading and parsing flexible
  def loadFromFile(file : File, p : Parsers, loader: FileLoader) : Option[Paper] = {

    // Check if file is bad or contains non numerical values (in the name)
    if (checkIfBad(file) || """[^0-9]+""".r.findFirstIn(SystemHelper.name(file.getName())).isDefined) return None
    
    val result = loader.loadFromFile(file, p)
    
    // If paper doesn't exist and didn't parse, let's not parse it again
    if(result == None) return isBadFile(file)
    else Cache.save(result.get.clean, Cache.parsed)          // Save and return
    
    return result
  }

  
  def isBadFile(file: File): Option[Paper] = {
      println("Couldn't parse " + file.getName())
      Cache.bad(file)
      return None
  }
  
  def checkIfBad(file : File) : Boolean = {

    // Get file
    val cached = new File(Cache.dir + file.getName + "." + Cache.bad)
    
    // Check that file exists
    return (cached.exists)
  }

  // Loads a paper from cache
  def loadFromCache(file : File, postfix : List[String]) : Option[Paper] = {
    // Helper function to load file
    def getCache(postfix : String) : Option[Paper] = {
      // Get file
      val cached = new File(Cache.dir + file.getName + "." + postfix)
      
      // Check that file exists
      if (!cached.exists) return None

      // Else, load away
      return Some(Cache.load(cached))
    }

    postfix match {
      // No postfix? - Return None
      case Nil            => None
      // a postfix, well, let's try it
      case p::ps          => {
        val ret = getCache(p)
        // If we didn't find anything in cache, recurse
        if (ret == None) loadFromCache(file, ps)
        // Else return result
        else return ret
      }
    }
  }
  
  
}
