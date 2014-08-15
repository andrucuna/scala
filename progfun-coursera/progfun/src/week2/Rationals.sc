package week2

/**
 * Lectures 2.5 & 2.6
 * Worksheet introducing the new concept of data structures (classes and objects)
 *
 * Here we are defining the Rationals arithmetic!! :3
 */
object Rationals
{
  def x = new Rational( 11, 3 )                   //> x: => week2.Rational
  def y = new Rational( 6, 4 )                    //> y: => week2.Rational
  def z = new Rational( 2 )                       //> z: => week2.Rational
	x - y - z                                 //> res0: week2.Rational = 1/6
	x < y                                     //> res1: Boolean = false
	x max y                                   //> res2: week2.Rational = 11/3
	z                                         //> res3: week2.Rational = 2/1
}


// Rationals arithmetic
class Rational( x: Int, y: Int )
{
	require( y != 0, "denominator must be non-zero" )
	def this( x: Int ) = this( x, 1 )
	
	//We simplify the rational number on its initialization
	val numer = x / gcd( x, y )
	val denom = y / gcd( x, y )
	
	
	// Methods
	def unary_- ( ): Rational = new Rational( (-1)*numer, denom )
	
	def + ( r: Rational ): Rational = new Rational( r.numer * denom + numer * r.denom, r.denom * denom )
	
	def - ( r: Rational ): Rational = this + -r
	
	def < ( r: Rational ): Boolean = numer * r.denom < r.numer * denom
	
	def max( that: Rational ): Rational = if( this < that ) that else this
	
	override def toString( ): String = numer + "/" + denom
	
	
	// Private methods for implementation purposes
	private def gcd( a: Int, b: Int ): Int = if( b==0 ) a else gcd( b, a%b )
}