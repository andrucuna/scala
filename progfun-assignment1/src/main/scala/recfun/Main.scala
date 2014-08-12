package recfun
import common._

object Main 
{
  def main(args: Array[String]) 
  {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1:
   * Here, I simply determine if the number its on an edge of the triangle...
   * If not I sum the two numbers above it.
   */
  def pascal(c: Int, r: Int): Int = 
  {
    if( r == 0 ) 1
    else 
      if( c == 0 || c == r ) 1
      else pascal( c-1, r-1 ) + pascal( c, r-1 )
  }

  /**
   * Exercise 2:
   * Here I assign a +1 count to '(', -1 to ')' and 0 otherwise.
   * If I encounter a negative count anytime, then String its unbalanced.
   * When recursion finish balanceCount must be zero!
   */
  def balance(chars: List[Char]): Boolean = 
  {
    def charCount( char:Char ): Int = 
      if( char == '(' ) 1
      else if( char == ')' ) -1
      else 0
    
    def balanceCount(subChars: List[Char], acc:Int): Boolean =
      if( subChars.isEmpty ) acc == 0
      else if( acc < 0 ) false
      else balanceCount( subChars.tail, acc + charCount(subChars.head) )
    
    balanceCount( chars, 0 )
  }

  /**
   * Exercise 3:
   * This problem is equivalent to:
   * Given N and a set of integers S = { S1, S2, ..., Sm }, how many ways can we express
   * N as a linear combination of S with non-negative coefficients.
   * 
   * Look for the explanation of the problem thru dynamic programming in:
   * www.algorithmist.com/index.php/Coin_Change
   */
  def countChange(money: Int, coins: List[Int]): Int = 
  {
    if( money == 0 ) 1
    else if( money < 0 ) 0
    else if( coins.size <= 0 && money >= 1 ) 0
    else countChange( money, coins.tail ) + countChange(money-coins.head, coins )
  }
}
