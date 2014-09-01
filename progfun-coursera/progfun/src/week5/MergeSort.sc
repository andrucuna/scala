package week5

/**
 * Lecture 5.2
 *
 * Pairs & Tuples
 * 1st Version
 * Merge sort is more efficient than insertion sort on functional programming
 */
object MergeSort
{
		def msort( xs: List[Int] ): List[Int] =
		{
				val n = xs.length/2
				if( n == 0 ) xs
				else
				{
						val (fst, snd) = xs splitAt n
						merge( msort(fst), msort(snd) )
				}
		}                                 //> msort: (xs: List[Int])List[Int]
		
		def merge( xs: List[Int], ys: List[Int] ): List[Int] = xs match
		{
				case Nil => ys
				case x :: xs1 => ys match
				{
						case Nil => xs
						case y :: ys1 => if( x<y ) x :: merge( xs1, ys )
														 else y :: merge( xs, ys1 )
				}
		}                                 //> merge: (xs: List[Int], ys: List[Int])List[Int]
		
		
		msort( List( 5, 0, 7, 3, 15, 4 ) )//> res0: List[Int] = List(0, 3, 4, 5, 7, 15)
}