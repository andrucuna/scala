package week1

object NewtonsSqrt
{
  def abs(x:Double) = if(x<0) -x else x           //> abs: (x: Double)Double
  
  def sqrt(x:Double) =
 	{
  	def sqrtIter(guess: Double): Double =
  		if (isGoodEnough(guess)) guess
			else sqrtIter(improve(guess))
	
		def isGoodEnough(guess: Double) =
			abs(guess * guess - x ) / x < 0.00000000001
		def improve(guess: Double) =
			(guess + x / guess) / 2
		
	 	sqrtIter(1.0)
	}                                         //> sqrt: (x: Double)Double
	
	sqrt(2)                                   //> res0: Double = 1.4142135623746899
	sqrt(9)                                   //> res1: Double = 3.0
	sqrt(0.001)                               //> res2: Double = 0.03162277660168433
	sqrt(1e-20)                               //> res3: Double = 1.000000000002308E-10
	sqrt(1e20)                                //> res4: Double = 1.0000000000023079E10
}