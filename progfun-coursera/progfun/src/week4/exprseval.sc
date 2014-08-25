package week4

import week4.Expr._


/**
 * Lecture 4.6
 *
 * We are introducing pattern matching to show expressions.
 * Here I added case classes 'Var' for variables x and 'Prod' for products x*y asdiscussed previously.
 * I changed show function so it also deals with products.
 * The hard part is to pay attention how to get operator precedence right but to use as few parentheses as possible.
 *
 * Example:
 * Sum(Prod(2, Var("x")), Var("y")) should print as '2 * x + y'.
 * But Prod(Sum(2, Var("x")), Var("y")) should print as '(2 * x) + y'.
 *
 * To achieve parenthesis precedence correctly, I added trait 'Terminal' in class 'Expr' hierarchy!
 */
object exprseval
{
		def show( e: Expr ): String = e match
		{
				case Number( n ) => n.toString
				case Var( x ) => x
				case Sum( t1:Terminal, t2:Terminal ) => "(" + show( t1 ) + "+" + show( t2 ) + ")"
				case Sum( l, r ) => show( l ) + "+" + show( r )
				case Prod( lp, rp ) => show( lp ) + "*" + show( rp )
		}                                 //> show: (e: week4.Expr.Expr)String
		
		show( Sum( Prod( Number(2), Var("x")), Var("y") ) )
                                                  //> res0: String = 2*x+y
    show( Prod( Sum( Number(2), Var("x")), Var("y") ) )
                                                  //> res1: String = (2+x)*y
}