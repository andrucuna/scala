package week4

/**
 * Lecture 4.5
 * 
 * A trait to test object decomposition in scala
 */
trait ExprDcmp
{
	def isNumber: Boolean
	def isSum: Boolean
	def numValue: Int
	def leftOp: ExprDcmp
	def rightOp: ExprDcmp
  
	def eval: Int
}

case class NumberDcmp( n: Int ) extends ExprDcmp
{
	def isNumber = true
	def isSum = false
	def numValue = n
	def leftOp = throw new Error( "Number.leftOp" )
	def rightOp = throw new Error( "Number.rightOp" )
  
	def eval: Int = n
}

case class SumDcmp( e1: ExprDcmp, e2: ExprDcmp ) extends ExprDcmp
{
	def isNumber = false
	def isSum = true
	def numValue = throw new Error( "Sum.numValue" )
	def leftOp = e1
	def rightOp = e2
  
	def eval: Int = e1.eval + e2.eval
}