package week4


/**
 * Lecture 4.1
 * 
 * Functions as objects.
 * Extending definition in lecture 3.3
 */
trait List[T]
{
	def isEmpty: Boolean
	def head: T
	def tail: List[T]
	def nth( n: Int ): T
}


class Cons[T]( val head: T, val tail: List[T] ) extends List[T]
{
	def isEmpty: Boolean = false
	def nth( n: Int ): T = if( n == 0 ) head else tail.nth( n-1 )
}

class Nil[T] extends List[T]
{
	def isEmpty: Boolean = true
	def head: Nothing = throw new NoSuchElementException( "Nil.head" )
	def tail: Nothing = throw new NoSuchElementException( "Nil.tail" )
	def nth( n: Int ): Nothing = throw new IndexOutOfBoundsException
}

object List
{
	//List( 1, 2 ) = List.apply( 1, 2 )
	def apply[T]( x1: T, x2: T ): List[T] = new Cons( x1, new Cons( x2, new Nil ) )
	
	//List( 3 ) = List.apply( 3 )
	def apply[T]( x1: T ): List[T] = new Cons( x1, new Nil )
	
	//List( ) = List.apply( )
	def apply[T](): List[T] = new Nil
}