package week5

/**
 * Lecture 5.1
 *
 * Here, we are studying more methods on lists like:
 * - xs.length
 * - xs.last
 * - xs.init
 * - xs take n
 * - xs drop n
 * - xs(n)
 */
object MoreOnLists
{
		/**
		 * Last is a function of complexity O(N)
		 */
		def last[T]( xs: List[T] ): T = xs match
		{
		  	case List() => throw new Error( "last of empty list" )
		  	case List(x) => x
		  	case y :: ys => last(ys)
		}                                 //> last: [T](xs: List[T])T
		
		/**
		 * Init is a function of complexity O(N)
		 */
		def init[T]( xs: List[T] ): List[T] = xs match
		{
		  	case List() => throw new Error( "init of empty list" )
		  	case List(x) => List()
		  	case y :: ys => y :: init(ys)
		}                                 //> init: [T](xs: List[T])List[T]
		
		/**
		 * Concat is a function of complexity O(N), where N = |xs|
		 */
		def concat[T]( xs: List[T], ys: List[T] ): List[T] = xs match
		{
		  	case List() => ys
		  	case z :: zs => z :: concat( zs, ys )
		}                                 //> concat: [T](xs: List[T], ys: List[T])List[T]
		
		/**
		 * Reverse is a function of complexity O(N^2)
		 */
		def reverse[T]( xs: List[T] ): List[T] = xs match
		{
		  	case List() => xs
		  	case y :: ys => reverse( ys ) ++ List(y)
		}                                 //> reverse: [T](xs: List[T])List[T]
		
		/**
		 * This function returns the List xs, removing the element at Nth position:
		 * remoAt( 1, List( 'a', 'b', 'c', 'd' ) ) = List( 'a', 'c', 'd' )
		 */
		def removeAt[T]( n: Int, xs: List[T] ): List[T] = (xs take n) ::: (xs drop n)
                                                  //> removeAt: [T](n: Int, xs: List[T])List[T]
		
		
		/**
		 * This function takes any argument and returns a flatten list, taken just the single elements.
		 * flatten(List(List(1, 1), 2, List(3, List(5, 8)))) = List[Any] = List(1, 1, 2, 3, 5, 8)
		 */
		def flatten(xs: List[Any]): List[Any] = xs match
		{
		  	case List() => List()
		  	case y :: ys => y match
		  	{
		  			case z :: zs => List(z) ::: flatten(zs) ::: flatten(ys)
		  			case w => List(w) ::: flatten(ys)
		  	}
		}                                 //> flatten: (xs: List[Any])List[Any]
		
		flatten(List(List(1, 1), 2, List(3, List(5, 8))))
                                                  //> res0: List[Any] = List(1, 1, 2, 3, 5, 8)
}