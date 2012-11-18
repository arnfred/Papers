package paper
import scala.collection.immutable.List


 trait BagOfWords {


	//compare based on scores and return List[Paper]
	def compareBoW(paperPos: String, papers : Option[List[Paper]], limit : Int) : List[Paper] = {
	  val loadedPapers = if(papers == None) CacheLoader.load(paperPos, Cache.extended) else papers.get
	  val matrixOfWeights: Array[Array[Int]] = getMatrixOfScores(loadedPapers)
			loadedPapers.map(p => {
				// Check that paper isn't already linked
				if (p.meta.get("linked") == None) {
					// Get list of papers that aren't current paper
					val otherPapers = loadedPapers.filter(p != _)

					// Compare to every other paper
					// Test					
					val weights : List[Int] = for (other <- otherPapers) yield getScores(matrixOfWeights, p.index)(other.index)
					// Make links
					//val links = for ((p,w) <- otherPapers.zip(weights) if w >= limit) yield Link(p.id,w)
					val links = for ((p,w) <- otherPapers.zip(weights) if w >= limit) yield Link(p.id,w)

					// Add links to paper, and set it as linked
					val result = p.setLinks(links).setMeta("linked", "yes")

					// Save result
					Cache.save(result, Cache.linked)

					result
				}
				else p
			})
	}

	def getScores(matrixOfScores: Array[Array[Int]], column: Int): List[Int] ={
	  
	  val matrixOfScoresTranspose = matrixOfScores.transpose
	  
	  return matrixOfScoresTranspose(column).toList

	}
	def getMatrixOfScores(papers: List[Paper]): Array[Array[Int]] ={

			//val filesList = new java.io.File(directory.toString).listFiles.filter(_.getName.endsWith(".txt"))

			val datasetSize = papers.length
			//convert dataset size to float to avoid errors
			datasetSize.toFloat

					//Initialisation of arrays
					//Array storing the different sources and the different texts
					val source = new Array[scala.io.BufferedSource](papers.length)
					//we will be changing the content of text -> create it as variable
					var text = new Array[java.lang.String](papers.length)

					val occurences = new Array[Map[java.lang.String,Array[java.lang.String]]](papers.length)
					//now we want to have a map between words and the number of occurences
					//create an array for easier manipulation
					val counts = new Array[Map[java.lang.String,Int]](papers.length)
					
					//Create an array of lists to store all different lists of keys:
					val countsList = new Array[List[java.lang.String]](papers.length)

					//List holding all the list of strings of all the texts
					var textsList = List[java.lang.String]()
					//reading from every entry of the list:
					for (k <- 0 to papers.length-1){

						//source(k) = scala.io.Source.fromFile(papers(k))
						//text(k) = source(k).mkString
					    text(k) = papers(k).getAbstract.getText
						//leave out unecessary characters from the analysis
						text(k) = clean(text(k))
						//Splitting the string into words to add stemming to every single word
						val splitString = text(k).split("\\s")
						var stemmedString = new Array[java.lang.String](splitString.length)
						var i = 0
						splitString foreach {e=>
			  								val a = breeze.text.analyze.PorterStemmer.apply(e)	
			  								//There is still a blank space at the beginning of string (does not affect output)
			  								stemmedString(i) = a
			  								i+=1
		  									}		
						//source(k).close()
						counts(k) = stemmedString.groupBy(x=>x).mapValues(x=>x.length)
						
						//counts(k) foreach{e=>
						// val a = clean(e._1)
						// newCounts(k).+((a,e._2))}
						//only working with keys for now, creating a list of keys for every text:
						countsList(k) = counts(k).keys.toList		
						
						if(k == 0){
							textsList = countsList(k)
							
						}else{
							textsList = textsList ::: countsList(k)
							
						}						
					}

					//println(counts.deep.mkString("\n"))
					//building dictionary:
					//find unique words in texts:
					//val texts = textsList.flatten
					//var newtextsList = List[java.lang.String]()
					//textsList foreach {e => val a = breeze.text.analyze.PorterStemmer.apply(e) 
					//newtextsList = newtextsList ::: List(a)}
					//val textsLength = newtextsList.length
					val dictionary = textsList.distinct.sortWith(_<_)
					//println(dictionary)

					// we compute the array of scores for the vectors of words for every document
					val tfidfArray = new Array[Array[Double]](dictionary.length,datasetSize)

					println("Computing tfidf array... ")
					for (i <- 0 to dictionary.length -1){
						for (j <- 0 to datasetSize -1){
							//compute tfidf value for word i and document j
							tfidfArray(i)(j) = tfidf(dictionary(i),j,datasetSize,counts)
							//println(tfidfArray(i)(j))
						}
					}
					println("Computing tfidf array: Complete...")
					//once we have the scores we can compute the absolute distance between papers and classify them
					//This is performed computing a scalar product on the score vectors for every document
					//Computation might take some time
			
					//temporary while getting "scalala" to work:
					val scalarProduct = new Array[Array[Double]](datasetSize,datasetSize)
					val cosineSimilarity = new Array[Array[Double]](datasetSize,datasetSize)
					//transpose array to perform row Array operations instead of column based operations
					//println(tfidfArray.deep.mkString("\n"))
					val tfidfTranspose = tfidfArray.transpose
			
					val normalisationTerm = 1
					println("Computing scalar product array")
					for (i <- 0 to datasetSize -1){
						//println(i)
						for (j <- 0 to datasetSize -1){
						  if(i==j)
						    cosineSimilarity(i)(j) = 0
						  else{
							//May induce some computational time
							val normVectorI = math.sqrt(dotProduct(tfidfTranspose(i),tfidfTranspose(i)))
							val normVectorJ = math.sqrt(dotProduct(tfidfTranspose(j),tfidfTranspose(j)))
							//println("dot product of " + tfidfTranspose(i).deep.mkString("\n") +  " and " + tfidfTranspose(j).deep.mkString("\n") + " is " + dotProduct(tfidfTranspose(i),tfidfTranspose(j)))
				
						    //Here operations take cost of length O(dictionary length)
					     	//compute cosine similarity
							scalarProduct(i)(j) = dotProduct(tfidfTranspose(i), tfidfTranspose(j))
							//println(" " + normVectorI + " " + tfidfTranspose(i).deep.mkString("\n"))
							//println(scalarProduct(i)(j))
							cosineSimilarity(i)(j) = scalarProduct(i)(j)/(normVectorI*normVectorJ)
							//println(cosineSimilarity(i)(j) + " " + scalarProduct(i)(j) + " " + normVectorI + " " + normVectorJ)
							
						  }	
						}
			         }
					//return array of scores
					println("Computing scalar product array: Done...")			
					// map every score with the paper ID
					//for every paper sort according to scores
					println("Sorting accordingly...")
					val a = 0 until datasetSize
					var positions = new Array[List[(Double,Int)]](datasetSize)
					for(k <- 0 to datasetSize-1){
						positions(k) = (scalarProduct(k).zip(a)).toList.sortWith(_._1 < _._1 )
					}
					//check if rounding is done correctly
					val maximalWeight = cosineSimilarity.flatten.max
					//println(maximalWeight)
					val normalizedCosSimilarity = cosineSimilarity.map(col =>{
					  col.map(weight =>  ((weight*100)/maximalWeight).toInt)
					})
					//return sorted scores with according paper
					//println(cosineSimilarity.deep.mkString("\n"))
					
					normalizedCosSimilarity
					//(i,j) of scalarProduct represents the scalar product of document i and document j. Now we have
					// to sort it in order in a list to return the closest documents to a given document
					//we have weights (higher weight/score) means being closer document-to-document wise
	}



	// Code has to be made generic for any text file parsed and for the whole dataset to be accurate

	//reading text from given file
	//Loading the List of all available text files in directory



// working with lists:



	//building dictionnary:
	//find unique words in texts:


	//println("The total length of the dictionnary is given by: " + dictionary.length)

	//println("the total length of the list is: " + textsLength)

	//Computing TF value:

	def tf(term: String, document: Int, counts: Array[Map[java.lang.String,Int]]): Double = {
					//Without normalisation
					if (counts(document).contains(term)){
						val freq = counts(document)(term)
						val normalizedFreq = freq
					return normalizedFreq
					}else{
					return 0.0
					}
			//normalization with respect to the documents length to prevent any bias:
			//new normalisation with respect to the highest occurence in the document
			
	}

	//Computing IDF value

	def idf(term: String, datasetSize : Double, counts: Array[Map[java.lang.String,Int]]): Double = {
			//math.log(size / index.getDocCount(term))
			// take the logarithm of the quotient of the number of documents by the documents where term t appears
			var appearances = 0
			//convert appearances to a float (to avoid errors)
			appearances.toFloat
			//println(counts.deep.mkString("\n"))
			counts foreach {x => if (x.contains(term)){
									appearances += 1
									
						   }
			//println(term + " => appearances: " + appearances)
			}
			val a = math.log(datasetSize/appearances)  
			return a

	}

	def tfidf(term:String, document: Int, datasetSize : Double, counts: Array[Map[java.lang.String,Int]]) : Double = {
			//create tfidf matrix
			//tfidf = tf*idf
			
			val tfidf = tf(term,document,counts)*idf(term,datasetSize,counts)
			//println("For document " + document + " and word " + term + " the value of tf is " + tf(term,document,counts) + " and the value of idf is " + idf(term,datasetSize,counts))
					return tfidf

	}

	//defining scala product for array vector operations
	def dotProduct[T <% Double](as: Iterable[T], bs: Iterable[T]) = {
		require(as.size == bs.size)
		(for ((a, b) <- as zip bs) yield a * b) sum
	}

	//replace all characters of a string except for a-z or A-Z (replacing numbers) and finally _: 
	def clean(in : String) = { if (in == null) "" else in.replaceAll("[^a-zA-Z_]", " ").toLowerCase
	}
	

}
