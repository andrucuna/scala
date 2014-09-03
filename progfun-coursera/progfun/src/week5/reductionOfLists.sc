package week5

/**
 * Lecture 5.5
 *
 * Reduction of lists using reduceLeft & reduceRight, foldLeft & foldRight
 */
object reductionOfLists
{
		def mapFun[T, U](xs: List[T], f: T => U): List[U] =
    	(xs foldRight List[U]())( f( _ ) :: _ )   //> mapFun: [T, U](xs: List[T], f: T => U)List[U]

  	def lengthFun[T](xs: List[T]): Int =
    	(xs foldRight 0)( List(_).length + _ )    //> lengthFun: [T](xs: List[T])Int
    	
   	mapFun( List( 1, 2, 3, 4 ), (x: Int) => x*2 )
                                                  //> res0: List[Int] = List(2, 4, 6, 8)
   	lengthFun( List( 1, 2, 3, 4 ) )           //> res1: Int = 4
}