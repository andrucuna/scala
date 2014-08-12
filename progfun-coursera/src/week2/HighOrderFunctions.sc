package week2

/**
 * Lecture 2.1
 * A high order function: SUM f(n) a<=n<=b
 * Tail-recursive version
 */
object HighOrderFunctions
{
	def sum( f: Int => Int ) ( a: Int, b:Int ): Int =
	{
		def loop( a: Int, acc: Int ): Int =
		{
			if(a>b) acc
			else loop( a+1, f(a)+acc )
		}
		
		loop( a, 0 )
	}                                         //> sum: (f: Int => Int)(a: Int, b: Int)Int
	
	def sumInts(a: Int, b: Int) = sum(x => x) (a, b)
                                                  //> sumInts: (a: Int, b: Int)Int
	def sumCubes(a: Int, b: Int) = sum(x => x * x * x) (a, b)
                                                  //> sumCubes: (a: Int, b: Int)Int
          
  sumInts( 0, 5 )                                 //> res0: Int = 15
  sumCubes( 0, 3 )                                //> res1: Int = 36
}