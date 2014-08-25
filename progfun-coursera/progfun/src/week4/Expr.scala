package week4

/**
 * Lecture 4.6
 * 
 * New attemp of object decomposition: this time we are using pattern matching
 */
object Expr 
{
	trait Expr
	trait Terminal extends Expr
	case class Number( n: Int ) extends Terminal
	case class Var( x: String ) extends Terminal
	case class Sum( e1: Expr, e2: Expr ) extends Expr
	case class Prod( e1: Expr, e2: Expr ) extends Expr
}