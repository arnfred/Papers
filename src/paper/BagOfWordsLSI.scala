package paper


import breeze.linalg.DenseVector
import breeze.classify
import org.netlib.lapack.LAPACK
import org.netlib.util.intW
import breeze.linalg.support.{CanCopy}



trait bagOfWordsLSI {


 // error is here:
  def compareBoWLSI(paperPos: String, papers : Option[List[Paper]], limit : Int) : List[Paper] = {
	  val loadedPapers = if(papers == None) CacheLoader.load(paperPos, Cache.extended) else papers.get
	  val matrixOfWeights: breeze.linalg.DenseMatrix[Int] = createTDMatrix(loadedPapers,loadedPapers.length)
			loadedPapers.map(p => {
				// Check that paper isn't already linked
				if (p.meta.get("linked") == None) {
					// Get list of papers that aren't current paper
					val otherPapers = loadedPapers.filter(p != _)
					println(getScores(matrixOfWeights, p.index).toString)
					// Compare to every other paper
					// Problem is in this line			
					val weights : List[Int] = for (other <- otherPapers) yield getScores(matrixOfWeights,p.index).valueAt(other.index)
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
  
  def getScores(matrixOfScores: breeze.linalg.DenseMatrix[Int], column: Int): DenseVector[Int] ={

	  val matrixOfScoresTranspose = matrixOfScores.t

	  return matrixOfScoresTranspose(::,column)

	}

	//Methods to compute the term-document matrix 
	def tf(term: String, document: Int, counts: Array[Map[java.lang.String,Int]]): Double = {
		val keyValue = counts(document).values
		val normalisationTerm = keyValue.max
			//test without normalisation	
			if (counts(document).contains(term)){
				val freq = counts(document)(term)
				val normalizedFreq = freq
				///normalisationTerm					
				return normalizedFreq				
			}else{					 
				return 0.0
			}
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
		//Create tfidf matrix
			//tfidf = tf*idf
		val tfidf = tf(term,document,counts)*idf(term,datasetSize,counts)
		return tfidf
	}

	//Creating the matrix:
	def createTDMatrix(papers: List[Paper], approximation: Int): breeze.linalg.DenseMatrix[Int] = {
		val datasetSize = papers.length
		//val filesList = new java.io.File(directory.toString).listFiles.filter(_.getName.endsWith(".txt"))
		//val datasetSize = filesList.length
		datasetSize.toFloat
		//Initialisation of arrays
		//Array storing the different sources and the different texts
		val source = new Array[scala.io.BufferedSource](papers.length)
		var text = new Array[java.lang.String](papers.length)
		val occurences = new Array[Map[java.lang.String,Array[java.lang.String]]](papers.length)
		//now we want to have a map between words and the number of occurences
		//create an array for easier manipulation
		var counts = new Array[Map[java.lang.String,Int]](papers.length)
		var newCounts = new Array[Map[java.lang.String,Int]](papers.length)
		//Create an array of lists to store all different lists of keys:
		val countsList = new Array[List[java.lang.String]](papers.length)
		//List holding all the list of strings of all the texts
		var textsList = List[java.lang.String]()
		//reading from every entry of the list:

			for (k <- 0 to papers.length-1){
				//source(k) = scala.io.Source.fromFile(filesList(k))
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
				//Array of the words of the parsed text
			    //words = clean(words)
				//var stemmedWords = new Array[java.lang.String](words.length)	
				//Using stemmer for every text:
				//words foreach{e => 									
							 //stemmedWords(counter) = breeze.text.analyze.PorterStemmer.apply(e) 
							 //counter += 1
							 //}
				//occurences(k) = stemmedWords.groupBy(x=>x)
				// create a map of the keys of the text with their occurences
				//the problem was the split...
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

			//adding stemming to the text
			//var newtextsList = List[java.lang.String]()
				//	textsList foreach {e => val a = breeze.text.analyze.PorterStemmer.apply(e) 
				//	newtextsList = newtextsList ::: List(a)}
			//building dictionary:
			//find unique words in texts
			val textsLength = textsList.length
			val dictionary = textsList.removeDuplicates.sort(_<_)
			println(dictionary)
			// we compute the Matrix of scores for the vectors of words for every document
		    //construct it as a vector to convert it as a Matrix
			val tfidfVector = new Array[Double](dictionary.length*datasetSize)
			var j = 0 
			println("Computing tfidf vector... with a dictionary of length " + dictionary.length + " and wait up to " + dictionary.length*datasetSize)
				for (i <- 0 to dictionary.length*datasetSize-1){
					println(i)
					//compute tfidf value for word i and document j
					//check if we have reached the length of the dictionary we change document and compute values
					if (i % dictionary.length == 0 && i != 0){
						j += 1	
					}

			        tfidfVector(i) = tfidf(dictionary(i%dictionary.length),j,datasetSize,counts)			
				}

			println("Computing tfidf vector: Complete...")

			val termDocMatrix = new breeze.linalg.DenseMatrix[Double](dictionary.length,datasetSize,tfidfVector)
			println(termDocMatrix.toString)
			println("done")
			//once having the termDocMatrix, compute the SVD and then compute cosine similarity on the new matrix:


			//SVD method returns simple arrays - need to convert them:
			val (u,s,v) = svd(termDocMatrix)
			 //println(u.deep.mkString("\n"))
			//println("end of u")
			println(s.deep.mkString("\n"))
			println("end of s")
			//println(v.deep.mkString("\n"))
			//println("end of v")

			//converting w and d to 2 dimensional arrays:
			var uo = reconstructArray(u,termDocMatrix.rows)
			println(uo.length + " " + uo.transpose.length)
			var vo = reconstructArray(v,termDocMatrix.cols)
			println(vo.length + " " + vo.transpose.length)
			var so = Array.ofDim[Double](termDocMatrix.rows,termDocMatrix.cols)
			println(so.length + " " + so.transpose.length)
			var count = 0
			var count2 = 0

			// put the vector s into a n*m matrix so form:
			s foreach{e=>	 
			  if(count2 <= termDocMatrix.cols-1){
					so(count)(count2) = e		
					count += 1
					count2 += 1			
			  		}
			}

			val test = multiplyArrays(uo,multiplyArrays(so,vo))
			val newtest = new Array[Array[Double]](10,10)
			for(i<- 0 to 9){
			  for (j<- 0 to 9){
			    newtest(i)(j)=so(i)(j)
			  }
			}
			//printing s to see the values
			//println(s.deep.mkString("\n"))

			//keeping k greatest singular values:
			//test with half the length of the vector
			val emptyList = List[Int]()
			println("computing k maximal values")
			//need to make a local copy of s:
			//val so = copy(s)
			// SVD returns the first k values as the k highest values - assuming no need for indices:
			//val indices = findKMax(s, approximation, emptyList)

			println("computed k maximal values")
			//println(indices)
			//select values of the indices in the given array s - indices are the 0 to k-1 first values:
			val emptyArray = new Array[Array[Double]](0)
			val keptValues = selectElementsOf2dimArray(so,(0 to approximation-1).toList,emptyArray)
			var newKeptValues = new Array[Array[Double]](keptValues.length,keptValues.length)
			var arrayCounter = 0 

			println(keptValues.deep.mkString("\n"))
			//keptValues.transpose foreach { x=> arrayCounter match{
			  //case y if arrayCounter < indices.length  => newKeptValues(arrayCounter) = x
			  //case y if arrayCounter >= indices.length => x	
			  //arrayCounter += 1
			//}
			val keptVTr = keptValues.transpose
			keptVTr foreach{e =>
			if (arrayCounter <= approximation-1){
				newKeptValues(arrayCounter) = e
				arrayCounter += 1
			}else{
			  Nil
			}
			  }
			println("separation")
			println(newKeptValues.deep.mkString("\n"))

			//matrix multiplication: 
			//keep the same values for the matrix multiplication: XO = UO*SO*VtO 
			val emptyArray2 = new Array[Array[Double]](0)
			val emptyArray3 = new Array[Array[Double]](0)
			var newUo = selectElementsOf2dimArray(uo.transpose,(0 to approximation-1).toList,emptyArray2)
			newUo = newUo.transpose
			var newVo = selectElementsOf2dimArray(vo,(0 to approximation-1).toList,emptyArray3)
			newVo = newVo.transpose
			//Multiplication:
			println(newUo.length,newUo.transpose.length,keptValues.length,keptValues.transpose.length,newVo.length,newVo.transpose.length)
			println(keptValues.length + " " + keptValues.transpose.length + " " + newVo.length + " " + newVo.transpose.length)
			val xo = multiplyArrays(newUo,multiplyArrays(newKeptValues,newVo))


			//currently performing test without newUo or newVo
			// Obtain a single dimension array out of the resulting array xo
			//var recomposedMatrix = new Array[Double](newUo.length*newVo.transpose.length)
			//var k = 0		
			//useless - use flatten method instead:
			//xo.transpose foreach { e => e foreach{ b=> recomposedMatrix(k) = b	
			//			k= k+1
			//			if(k-1<=20){
			//			println(recomposedMatrix(k))}		  
			//}
			//}
			//replaced xo by test
			val recomposedMatrix = xo.transpose.flatten
			val newrecomposedMatrix = test.transpose.flatten
			var counter = 0

			recomposedMatrix foreach{ e => 
			  if(counter<=30) {println("recomposed -> " + e)
			    println("tfidf -> " + tfidfVector(counter))
			    println("newrecomp -> " + newrecomposedMatrix(counter))} 
			  counter+=1}
			//println(recomposedMatrix.deep.mkString("\n"))

			// Compute distance between the two matrices and then compute the norm of the average distance

			//create an iterator that goes through the array:
			//Edit: does not work because termDocMatrix is a Matrix and not an array as per
			//Iterator.fromArray(xo) foreach (x => Iterator.fromArray(termDocMatrix) foreach (y => x.zip(y)))

			def matchApproximation(app: Int): breeze.linalg.DenseMatrix[Double] = app match{
			  case termDocMatrix.cols =>  new breeze.linalg.DenseMatrix[Double](termDocMatrix.rows,termDocMatrix.cols,newrecomposedMatrix)

			  case 0 => new breeze.linalg.DenseMatrix[Double](termDocMatrix.rows,termDocMatrix.cols,newrecomposedMatrix)

			  case y => new breeze.linalg.DenseMatrix[Double](termDocMatrix.rows,termDocMatrix.cols,recomposedMatrix)	            
			}

			val newtermDocMatrix = matchApproximation(approximation)
			val distanceMat = Math.sqrt((newrecomposedMatrix.toList.zip(tfidfVector.toList) foldLeft 0.0)((sum,t) => 
					  					 sum + (t._2-t._1)*(t._2-t._1)))			
			//compute cosine similarity:
			//println(newtermDocMatrix.toString)
			val similarityMatrix = breeze.linalg.DenseMatrix.zeros[Double](datasetSize,datasetSize)

			//not optimal version:
				for (i <- 0 to datasetSize-1){
					for (j <- 0 to datasetSize-1){
						if(i==j){
					    similarityMatrix(i,j) = -1
						}else{
						//Compute scalar product between two matrices
						  // maybe perform transpose?
						val firstColumn = newtermDocMatrix(0 to newtermDocMatrix.rows-1,i)

						val secondColumn =  newtermDocMatrix(0 to newtermDocMatrix.rows-1,j)
						//println(firstColumn.toString,i)
						similarityMatrix(i,j) = firstColumn.dot(secondColumn)	 

					    //Compute 2nd norm and output cosine similarity
						val firstColumnNorm = firstColumn.norm(2)
						//println(firstColumnNorm)
						val secondColumnNorm = secondColumn.norm(2)
						//test without normalisation:
						similarityMatrix(i,j) = similarityMatrix(i,j)/(firstColumnNorm*secondColumnNorm)
						println(i + " and j is " + j )
						println(similarityMatrix(i,j))
					    }
					}
				}
			val maximalWeight = similarityMatrix.max
			//val smat = similarityMatrix.toString
			//1 way to do it
			println(distanceMat)
			val normalizedCosSimilarity = similarityMatrix.map(weight =>{
												((weight*100)/maximalWeight).toInt
															})

			return normalizedCosSimilarity
			//Code works but there are several problems: NaN for some similarity values... not normal
			// NaN values are due to the division of 0 by a really high number... Need to perform tests.
	}

	/*
def getScores(matrixOfScores: DenseMatrix[Double], column: Int): List[Double] ={

		//val matrixOfScoresTranspose = matrixOfScores.transpose

		//	  return matrixOfScoresTranspose(column).toList

}
	 */
	def exportMatrixToText(matrix: String) : Unit = {
    val file = new java.io.File("exportToGephi.csv")
    val p = new java.io.PrintWriter(file)
    p.println(matrix)
    p.close
  }
	//remove an element on a given index from a given list:
	def dropIndex[T](xs: List[T], n: Int) = {
		val (l1, l2) = xs splitAt n
				l1 ::: (l2 drop 1)
	}

	def selectElementsOfArray(inputArray: Array[Double], inputIndices: List[Int], returnArray: Array[Double]): Array[Double] = {
		//var returnArray = new Array[Double](inputIndices.length)
		if(inputIndices == Nil){
			inputArray
		}else if(inputIndices.length != 1){		
			//Add element of the input array corresponding to the first index in the index list:
			var newreturnArray = returnArray :+ inputArray(inputIndices(0))		
					selectElementsOfArray(inputArray,dropIndex(inputIndices,0),newreturnArray)
		}else{
			var newreturnArray = returnArray :+ inputArray(inputIndices(0))	
					return newreturnArray					
		}		
	}

		def dotProduct[T <% Double](as: Iterable[T], bs: Iterable[T]) = {
		require(as.size == bs.size)
		(for ((a, b) <- as zip bs) yield a * b) sum
	}

	def selectElementsOf2dimArray(inputArray: Array[Array[Double]], inputIndices: List[Int], returnArray: Array[Array[Double]]): Array[Array[Double]] = {
		//var returnArray = new Array[Double](inputIndices.length)
		if(inputIndices == Nil){
			inputArray
		}else if(inputIndices.length != 1){		
			//Add element of the input array corresponding to the first index in the index list:
			var newreturnArray = returnArray :+ inputArray(inputIndices(0))		
			selectElementsOf2dimArray(inputArray,dropIndex(inputIndices,0),newreturnArray)
		}else{
			var newreturnArray = returnArray :+ inputArray(inputIndices(0))	
			return newreturnArray					
		}		
	}

	def clean(in : String) ={  if (in == null) "" else in.replaceAll("[^a-zA-Z-']"," ").toLowerCase}

	//computing find method to return the k largest indices of elements in a vector
	//temporary / must reduce complexity (perform tests etc...)
	//deletes maximal values of original array (to fix). Otherwise works perfectly // 
	def findKMax(inputVector: Array[Double], k: Int, listOfIndex: List[Int]): List[Int]= {
		if(listOfIndex.length < k){
			val maxofArray = inputVector.max
				if (maxofArray != 0){
					var newlistOfIndex = listOfIndex:::List(inputVector.findIndexOf(x => x == maxofArray))				
					//set maximal value to 0 so it does not get taken into account again
					inputVector(inputVector.findIndexOf(x => x == maxofArray)) = 0
					findKMax(inputVector,k, newlistOfIndex)

				}else{
					listOfIndex
				}
		}else{
			return listOfIndex
		}
	}
	// finds the N maximal values (not the indices)
	def topNs(xs: Array[Double], n: Int) = {
		var ss = List[Double]()
		var min = Double.MaxValue
		var len = 0
		xs foreach { e =>
						if (len < n || e > min) {
						ss = (e :: ss).sorted
						min = ss.head
						len += 1
						}
						if (len > n) {
						ss = ss.tail
						min = ss.head
						len -= 1
						}                    
					}
		ss
	} 	

	// to do: fix output
	def indexOftopNs(xs: Array[Double], n: Int) = {
		var ss = List[Int]()
		var min = Double.MaxValue
		var len = 0
		xs foreach { e =>
						if (len < n || e > min) {
						ss = (xs.findIndexOf(x=>x==e) :: ss).sorted
						min = ss.head
						len += 1
						}
						if (len > n) {
					    ss = ss.tail
					    min = ss.head
					    len -= 1
						}                    
					}
		ss
	} 	


	// function converting a DenseVector to a List of Double
	def convertToList(inputVector : DenseVector[Double]) :  List[Double] = {
		val outputVector = List[Double]()
		for(i <- inputVector){
			outputVector:::List(i)
		}
		return outputVector
	}

	//desiredLength = length of the rows (= number of columns)
	def reconstructArray(inputArray: Array[Double], desiredLength: Int)={
		val doubleDimArray = Array.ofDim[Double]((inputArray.length/ desiredLength).toInt,desiredLength)
		var k = 0
		var i = 0
		inputArray foreach { e =>  			 
								if(k < desiredLength){
									doubleDimArray(k)(i) = e
									k = k+1
								}
								if(k == desiredLength){
								k = 0
								i = i+1
								}			  
							}
		doubleDimArray					 
	}
	//redifining svd:

	//@inline private def requireNonEmptyMatrix[V](mat: Matrix[V]) =
	//if (mat.cols == 0 || mat.rows == 0)
	//throw new MatrixEmptyException
//modification of the www.netlib.org/lapack/ package, dgesdd method - derived from breeze svd
	def svd(mat: breeze.linalg.DenseMatrix[Double]):(Array[Double],Array[Double],Array[Double]) = {
		// we do not use the matrix requirements
		//	requireNonEmptyMatrix(mat)

		val m = mat.rows
		val n = mat.cols
		//val S = DenseVector.zeros[Double](m min n)
		//matrix of zeros
		//S = denseVector(UCOL = min(m,n))
		//val S = Matrix(m min n,1){ (i:Int,j:Int) => 0 }
		val S = new Array[Double](m min n)
		//val U = Matrix(m,m){ (i:Int,j:Int) => 0 }
		//val U = breeze.linalg.DenseMatrix.zeros[Double](m,m)
		val U =  new Array[Double](m*m)
		val Vt = new Array[Double](n*n)
		//Matrix(n,n){ (i:Int,j:Int) => 0 }
		val iwork = new Array[Int](8 * (m min n) )
		val workSize = ( 3
						* scala.math.min(m, n)
						* scala.math.min(m, n)
						+ scala.math.max(scala.math.max(m, n), 4 * scala.math.min(m, n)
						* scala.math.min(m, n) + 4 * scala.math.min(m, n))
						)
		val work = new Array[Double](workSize)
		val info = new intW(0)
		//S.elements.flatten.toArray
		//U.take(m*m).flatten.toArray
		val cm = copy(mat)
		println("im in")
		LAPACK.getInstance.dgesdd(
				"A", m, n,
				cm.data, scala.math.max(1,m),
				S, U , scala.math.max(1,m),
				Vt, scala.math.max(1,n),
				work,work.length,iwork, info)

		if (info.`val` > 0)
			throw new NotConvergedException(NotConvergedException.Iterations)
		else if (info.`val` < 0)
			throw new IllegalArgumentException()

		(U,S,Vt)
	}
	def copy[T](t: T)(implicit canCopy: CanCopy[T]): T = canCopy(t)

	class MatrixEmptyException extends IllegalArgumentException("Matrix is empty")

	class NotConvergedException(val reason: NotConvergedException.Reason, msg: String = "")
	extends RuntimeException(msg)

	object NotConvergedException {
		trait Reason
		object Iterations extends Reason
		object Divergence extends Reason
		object Breakdown extends Reason
	}

	//Scala for java developers: http://blog.scala4java.com/2011/12/matrix-multiplication-in-scala-single.html

	def multiplyArrays(m1: Array[Array[Double]], m2: Array[Array[Double]]) :  Array[Array[Double]] = {
    val res =  Array.ofDim[Double](m1.length, m2(0).length)
    val M1_COLS = m1(0).length
    val M1_ROWS = m1.length
    val M2_COLS = m2(0).length

    @inline def singleThreadedMultiplicationFAST(start_row:Int,  end_row:Int) {
      var col, i  = 0
      var sum = 0.0
      var row = start_row

      // while statements are much faster than for statements
      while(row < end_row){ col = 0
        while(col < M2_COLS){ i = 0; sum = 0
          while(i<M1_COLS){
            sum += m1(row)(i) * m2(i)(col)
            i+=1
          }

          res(row)(col) = sum
          col += 1

        }; row += 1
      }
    }

    (0 until M1_ROWS).par.foreach( i =>
      singleThreadedMultiplicationFAST(i, i+1)
    )

    res

  }

}
