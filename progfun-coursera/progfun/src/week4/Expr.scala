package week4

/**
 * Lecture 4.5
 * 
 * A trait to test object decomposition in scala
 */
trait Expr 
{
	def isNumber: Boolean
	def isSum: Boolean
	def numValue: Int
	def leftOp: Expr
	def rightOp: Expr
  
	def eval: Int
}

class Number( n: Int ) extends Expr
{
	def isNumber = true
	def isSum = false
	def numValue = n
	def leftOp = throw new Error( "Number.leftOp" )
	def rightOp = throw new Error( "Number.rightOp" )
  
	def eval: Int = n
}

class Sum( e1: Expr, e2: Expr ) extends Expr
{
	def isNumber = false
	def isSum = true
	def numValue = throw new Error( "Sum.numValue" )
	def leftOp = e1
	def rightOp = e2
  
	def eval: Int = e1.eval + e2.eval
}