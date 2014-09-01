package week5

import math.Ordering

/**
 * Lecture 5.3
 *
 * In this new version we are parameterizing the compare function. That way we can use
 * merge sort with generic types T, instead of using Ints.
 */
object MergeSortParameterization
{
  	def msort[T]( xs: List[T] )( implicit ord: Ordering[T] ): List[T] =
		{
				val n = xs.length/2
				if( n == 0 ) xs
				else
				{
						def merge( xs: List[T], ys: List[T] ): List[T] = (xs, ys) match
						{
								case ( Nil, ys ) => ys
								case ( xs, Nil ) => xs
								case ( x :: xs1, y :: ys1 ) => if( ord.lt(x, y) ) x :: merge( xs1, ys )
																						   else y :: merge( xs, ys1 )
						}
						
						val (fst, snd) = xs splitAt n
						merge( msort(fst), msort(snd) )
				}
		}                                 //> msort: [T](xs: List[T])(implicit ord: scala.math.Ordering[T])List[T]
		
		val nums = List( 5, 0, 7, 3, 15, 4 )
                                                  //> nums  : List[Int] = List(5, 0, 7, 3, 15, 4)
		val fruits = List( "apple", "pinapple", "orange", "banana" )
                                                  //> fruits  : List[String] = List(apple, pinapple, orange, banana)
		
		msort( nums )                     //> res0: List[Int] = List(0, 3, 4, 5, 7, 15)
    msort( fruits )                               //> res1: List[String] = List(apple, banana, orange, pinapple)
}