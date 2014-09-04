package week6

import scala.Range


/**
 * Lecture 6.1
 *
 * Other collections
 */
object otherCollections
{
	//This function tests if a number N is prime, its implementation is focused on
	//mathematical abstraction over its efficiency
  def isPrime(n: Int): Boolean = (2 until n) forall ( d => n % d != 0 )
                                                  //> isPrime: (n: Int)Boolean
                                                  
  isPrime( 2 )                                    //> res0: Boolean = true
  isPrime( 8 )                                    //> res1: Boolean = false
  isPrime( 13 )                                   //> res2: Boolean = true
  isPrime( 49 )                                   //> res3: Boolean = false
}