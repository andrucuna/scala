package week2

import math.abs


/**
 * Lecture 2.3
 * Finding fixed points on functions
 */
object FixedPoints
{
  val tolerance = 1e-4                            //> tolerance  : Double = 1.0E-4
  
  def isCloseEnough( x: Double, y: Double ) =
  	abs( (x-y) / x ) / x < tolerance          //> isCloseEnough: (x: Double, y: Double)Boolean
  
  def fixedPoint( f: Double => Double )( firstGuess: Double ) =
  {
  	def iterate( guess: Double ): Double =
  	{
  		val next = f(guess)
  		if( isCloseEnough( guess, next ) ) next
  		else iterate( next )
  	}
  	iterate( firstGuess )
  }                                               //> fixedPoint: (f: Double => Double)(firstGuess: Double)Double
  
  fixedPoint( x => 1 + x/2 )( 1 )                 //> res0: Double = 1.999755859375
  
  
  //We define the averageDamp function, because sqrt doesn't converge using fixedPoint method
  def averageDamp( f: Double => Double )( x: Double ) =
  	( x + f(x) ) / 2                          //> averageDamp: (f: Double => Double)(x: Double)Double
  	
  //Second version of sqrt, using 'fixed point' and 'average damp' method
  def sqrt( x: Double ) =
  	fixedPoint( averageDamp( y=> x/y ) )( 1 ) //> sqrt: (x: Double)Double
  
  
  sqrt( 2 )                                       //> res1: Double = 1.4142135623746899
}