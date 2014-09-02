package week5

/**
 * Lecture 5.4
 *
 * Introducing higher order functions on lists
 */
object ListsHigherOrder
{
		/**
		 * First common operation on lists: transforming all the elements on the list,
		 * given an operation.
		 */
		 
		// First version using pattern matching
		def scaleList( xs: List[Double], factor: Double ): List[Double] = xs match
		{
			case Nil => xs
			case y :: ys => y*factor :: scaleList( ys, factor )
		}                                 //> scaleList: (xs: List[Double], factor: Double)List[Double]
		
		// Second version using high order map function
		def scaleListMap( xs: List[Double], factor: Double ): List[Double] = xs map ( x => x*factor )
                                                  //> scaleListMap: (xs: List[Double], factor: Double)List[Double]
        
    //First version using pattern matching
    def squareList(xs: List[Int]): List[Int] = xs match
    {
	    case Nil     => xs
	    case y :: ys => y*y :: squareList(ys)
	  }                                       //> squareList: (xs: List[Int])List[Int]
  
    //Second version using high order map function
    def squareListMap(xs: List[Int]): List[Int] = xs map ( x => x*x )
                                                  //> squareListMap: (xs: List[Int])List[Int]
    
    
    /**
		 * Another common operation on lists: selecting all the elements on the list,
		 * that satisfies a given condition.
		 */
		 
		// First version using pattern matching
		def posElems(xs: List[Int]): List[Int] = xs match
    {
	    case Nil     => xs
	    case y :: ys => if( y > 0 ) y:: posElems(ys) else posElems(ys)
	  }                                       //> posElems: (xs: List[Int])List[Int]
		 
		// Second version using high order map function
		def posElemsFilter(xs: List[Int]): List[Int] = xs filter( x => x > 0 )
                                                  //> posElemsFilter: (xs: List[Int])List[Int]
}