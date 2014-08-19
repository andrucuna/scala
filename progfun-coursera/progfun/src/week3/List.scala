package week3

/**
 * Lecture 3.3
 * 
 * Implementing polymorphism using Lists as an example
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