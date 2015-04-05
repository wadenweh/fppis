package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(col: Int, row: Int): Int = 
      if (row == 0 || col == 0 || col == row) 1
      else if (col < 0 || col > row) 0
      else pascal(col, row - 1) + pascal(col - 1, row - 1)

  /**
   * Exercise 2
   */
  def balance(word: List[Char]): Boolean = {
    var w = word.filter(x => x == ')' || x == '(')
    if (w.head.equals(')') || w.last.equals('(')) false

    def innerbalance(c: Int, cword: List[Char]): Boolean = {
      if (c < 0) false
      else if (cword.isEmpty) c == 0
      else if (cword.head.equals('(')) innerbalance(c + 1, cword.tail)
      else innerbalance(c - 1, cword.tail)
    }
    innerbalance(0, w)
  }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int =
    {
      def countChangeInner(m: Int, c: List[Int]): Int =
        {
          if(m < 0 || c.isEmpty ) 0
          else if (m == 0) 1
          else countChangeInner(m - c.head, c) + countChangeInner(m, c.tail)
        }
      if(money < 1) 0
      else countChangeInner(money, coins)
    }
}
