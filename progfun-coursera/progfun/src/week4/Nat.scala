package week4

/**
 * Lecture 4.2
 * 
 * We are implementing a representation of natural numbers: Peano numbers
 */
abstract class Nat 
{
	def isZero: Boolean
	def predecessor: Nat
	def successor: Nat
	def + (that: Nat): Nat
	def - (that: Nat): Nat
}


//This object represents the first element of Nat
object Zero extends Nat
{
	def isZero: Boolean = true
	def predecessor: Nothing = throw new Error( "0.predecessor: zero has no predecessor" )
	def successor: Nat = new Succ( this )
	def + (that: Nat): Nat = that
	def - (that: Nat): Nat = if(that.isZero) this else throw new Error( "0.-: (-) can't yield negative values" )
}

//This class represents the next element of a given Nat
class Succ( n: Nat ) extends Nat
{
	def isZero: Boolean = false
	def predecessor: Nat = n
	def successor: Nat = new Succ( this )
	def + (that: Nat): Nat = new Succ( n + that )
	def - (that: Nat): Nat = if(that.isZero) n else n - that.predecessor
}

