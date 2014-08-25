package week4


/**
 * Lecture 4.7
 * 
 * An example of lists sorting, using insertion sort.
 * We introduce the concept of pattern matching on inmmutable structures like lists.
 */
class SortList 
{
	def isort( xs: scala.List[Int] ): scala.List[Int] = xs match
	{
	  case scala.List() => scala.List()
	  case y :: ys => insert( y, isort(ys) )
	}
	
	def insert( x: Int, xs: scala.List[Int] ): scala.List[Int] = xs match
	{
	  case scala.List() => scala.List(x)
	  case y :: ys => if( x <= y ) x :: xs else y :: insert( x, ys )
	}
}