package week2

/**
 * Lecture 2.5
 * Worksheet introducing the new concept of data structures (classes and objects)
 *
 * Here we are defining the Rationals arithmetic!! :3
 */
object Rationals
{
  def x = new Rational( 1, 3 )                    //> x: => week2.Rational
  def y = new Rational( 5, 7 )                    //> y: => week2.Rational
  def z = new Rational( 3, 2 )                    //> z: => week2.Rational
	x.sub( y ).sub( z )                       //> res0: week2.Rational = -79/42
}


// Rationals arithmetic
class Rational( x: Int, y: Int )
{
	def numer = x
	def denom = y
	
	
	// Methods
	def neg( ): Rational =
	{
		new Rational( (-1)*numer, denom )
	}
	
	def add( r: Rational ): Rational =
	{
		new Rational( r.numer * denom + numer * r.denom, r.denom * denom )
	}
	
	def sub( r: Rational ): Rational =
	{
		add( r.neg )
	}
	
	override def toString( ): String =
	{
		numer + "/" + denom
	}
}