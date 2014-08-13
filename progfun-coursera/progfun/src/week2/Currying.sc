package week2

/**
 * Lecture 2.2
 * Currying - High order functions
 *
 * Map Reduced function generalizes sum and product functions, taking arguments a and b
 * and calculates the result of operating points in interval
 */
object Currying
{
  def mapReduced( f: Int => Int, combine: (Int, Int) => Int, zero: Int )( a:Int, b: Int ): Int =
  {
  	if( a>b ) zero
  	else combine( f(a), mapReduced(f, combine, zero )( a+1, b ) )
  }                                               //> mapReduced: (f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b
                                                  //| : Int)Int
  
  def product( f: Int => Int )( a:Int, b: Int ): Int =
  {
  	if( a>b ) 1
  	else f(a) * product(f)(a+1, b)
  }                                               //> product: (f: Int => Int)(a: Int, b: Int)Int
  
  def productCurrying( f: Int => Int )( a: Int, b: Int ): Int =
  {
  	mapReduced( f, (x, y) => x*y, 1 )( a, b )
  }                                               //> productCurrying: (f: Int => Int)(a: Int, b: Int)Int
  
  def factorial( n: Int ) =  product( x => x )( 1, n )
                                                  //> factorial: (n: Int)Int
  
  def factorialCurrying( n: Int ) =  productCurrying( x => x )( 1, n )
                                                  //> factorialCurrying: (n: Int)Int
  
  factorial( 5 )                                  //> res0: Int = 120
  factorialCurrying( 5 )                          //> res1: Int = 120
}