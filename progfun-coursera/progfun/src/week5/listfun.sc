package week5


/**
 * Lecture 5.4
 * More functions on lists
 */
object listfun
{
  val nums = List( 2, -4, 5, 7, 1 )               //> nums  : List[Int] = List(2, -4, 5, 7, 1)
  val fruits = List( "apple", "pineapple", "orange", "banana"  )
                                                  //> fruits  : List[String] = List(apple, pineapple, orange, banana)
  
  nums filter ( x => x > 0 )                      //> res0: List[Int] = List(2, 5, 7, 1)
  nums filterNot ( x => x > 0 )                   //> res1: List[Int] = List(-4)
  nums partition ( x => x > 0 )                   //> res2: (List[Int], List[Int]) = (List(2, 5, 7, 1),List(-4))
  
  nums takeWhile( x => x > 0 )                    //> res3: List[Int] = List(2)
  nums dropWhile( x => x > 0 )                    //> res4: List[Int] = List(-4, 5, 7, 1)
  nums span ( x => x > 0 )                        //> res5: (List[Int], List[Int]) = (List(2),List(-4, 5, 7, 1))
  
  
  // This function pack that packs consecutive duplicates of list elements into sublists.
	def pack[T](xs: List[T]): List[List[T]] = xs match
	{
	   case Nil      => Nil
	   case x :: xs1 => List( x :: xs1 takeWhile( y => y == x ) ) ::: pack( xs1 dropWhile( y => y == x  ) )
 	}                                         //> pack: [T](xs: List[T])List[List[T]]
 	
 	// This function produces the run-length enconding of a list
 	def encode[T]( xs: List[T] ): List[(T, Int)] =
 	{
			val packedData = pack(xs)
			def encodeIter[T]( xs: List[List[T]] ): List[(T, Int)] = xs match
		 	{
		 			case Nil => List()
		 			case y :: ys => ( y.head, y.length ) :: encodeIter( ys )
		 	}
	 		encodeIter( packedData )
 	}                                         //> encode: [T](xs: List[T])List[(T, Int)]
 	
 	val data = List("a", "a", "a", "b", "c", "c", "a")
                                                  //> data  : List[String] = List(a, a, a, b, c, c, a)
 	pack( data )                              //> res6: List[List[String]] = List(List(a, a, a), List(b), List(c, c), List(a)
                                                  //| )
  encode( data )                                  //> res7: List[(String, Int)] = List((a,3), (b,1), (c,2), (a,1))
}