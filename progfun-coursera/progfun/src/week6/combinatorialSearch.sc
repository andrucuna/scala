package week6


/**
 * Lecture 6.2
 *
 * Combinatorial search & For-expressions
 */
object combinatorialSearch
{
	def isPrime(n: Int): Boolean = (2 until n) forall ( d => n % d != 0 )
                                                  //> isPrime: (n: Int)Boolean

	/* Given a positive integer n, find all pairs of positive integers
	 * i and j, with 1 <= j < i < n such that i+j is prime
	 *
	 * For example if n=7, the sougth pair are:
	 * 					i | 2 3 4 4 5 6 6
	 * 					j | 1 2 1 3 2 1 5
	 * 				--------------------
	 *				i+j | 3 5 5 7 7 7 11
	 */
	 val n = 7                                //> n  : Int = 7
  (1 until n) flatMap ( i => (1 until i) map ( j => (i, j) ) ) filter ( pair => isPrime( pair._1 + pair._2 ) )
                                                  //> res0: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,2
                                                  //| ), (4,1), (4,3), (5,2), (6,1), (6,5))
  
  // An equivalent expression that is more clear and concise, its constructed with a For-expression
  for
  {
  	i <- 1 until n
  	j <- 1 until i
  	if( isPrime( i+j ) )
  } yield( i, j )                                 //> res1: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,2
                                                  //| ), (4,1), (4,3), (5,2), (6,1), (6,5))
                                                  
                                                  
	//A version of scalarProduct that makes use of a for:
  def scalarProduct(xs: List[Double], ys: List[Double]) : Double =
	  (	for( (i, j) <- xs zip ys) yield( i*j ) ).sum
                                                  //> scalarProduct: (xs: List[Double], ys: List[Double])Double
}