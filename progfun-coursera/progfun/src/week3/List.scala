package week3

/**
 * Lecture 3.3
 * 
 * Implementing polymorphism using Lists as an example.
 * We make Nil an object by adding a covariant relationship between Nothing <: +T
 */
trait List[+T]
{
	def isEmpty: Boolean
	def head: T
	def tail: List[T]
	def nth( n: Int ): T
	def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
}


class Cons[T]( val head: T, val tail: List[T] ) extends List[T]
{
	def isEmpty: Boolean = false
	def nth( n: Int ): T = if( n == 0 ) head else tail.nth( n-1 )
}

object Nil extends List[Nothing]
{
	def isEmpty: Boolean = true
	def head: Nothing = throw new NoSuchElementException( "Nil.head" )
	def tail: Nothing = throw new NoSuchElementException( "Nil.tail" )
	def nth( n: Int ): Nothing = throw new IndexOutOfBoundsException
}