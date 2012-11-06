package paper
import java.io._
import scala.io.Source

trait ComparePaper {

  def compare(paperPos:String, papers : Option[List[Paper]], limit : Int) : List[Paper] = {
    println("BEGIN OF PAPERS COMPARISION")
	val loadedPapers = if(papers == None) CacheLoader.load(paperPos, Cache.extended) else papers.get
    
	val finalPapers = loadedPapers.map(p => {
      // Check that paper isn't already linked
      if (p.meta.get("linked") == None) {
        // Get list of papers that aren't current paper
        val otherPapers = loadedPapers.filter(p != _)

        // Compare to every other paper
        val weights : List[Int] = for (other <- otherPapers) yield getWeight(p, other)

        // Make links
        //val links = for ((p,w) <- otherPapers.zip(weights) if w >= limit) yield Link(p.id,w)
        val links = for ((p,w) <- otherPapers.zip(weights) if w >= 1) yield Link(p.index,w)

        // Add links to paper, and set it as linked
        val result = p.setLinks(links).setMeta("linked", "yes")

        // Save result
        Cache.save(result, Cache.linked)

        result
      }
      else p
    })
    println("END OF PAPERS COMPARISION")
    finalPapers
  }

  def getWeight(p : Paper, o : Paper) : Int = {
    // Get names
    val pNames = p.getDistinctNames
    val oNames = o.getDistinctNames

    // For each auther in p, check if he/she exists in other
    var matches = (for (name <- pNames if oNames.contains(name)) yield 1).sum

    // return result
    return (100 * matches.toDouble / pNames.length.toDouble).toInt

  }

}
