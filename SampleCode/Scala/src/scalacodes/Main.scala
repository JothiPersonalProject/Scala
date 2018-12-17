package sample.scalacodes

import scala.annotation.tailrec

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
   * Function to print Pascal's triangle
   */
    def pascal(c: Int, r: Int): Int = {
      factorial(r) / (factorial(c)*factorial(r-c))
    }

    def factorial(x: Int): Int = {
      @tailrec
      def factorialAccu(x: Int, sumAccu: Int): Int = {
        if (x == 0) sumAccu*1
        else factorialAccu(x-1,sumAccu*x)
      }

      factorialAccu(x,1)
    }
  /**
   * Finds whether the brackets are balanced in the given statement
   */
    def balance(chars: List[Char]): Boolean = {

      def openParen(x: Char): Boolean = x == '('
      def closeParen(x: Char): Boolean = x == ')'

      def balanceAccu(ls: List[Char], accu: Int): Int =  {

        if(accu < 0 || ls.isEmpty) accu
        else
        {
          if (ls.head == '(') balanceAccu(ls.tail,accu+1)
          else
          {
            if(ls.head == ')') balanceAccu(ls.tail,accu-1) else accu
          }
        }
      }

      if(!chars.isEmpty)
      {
        val lis = chars filter { x => openParen(x) || closeParen(x) }
        if(balanceAccu(lis,0) == 0) true
        else false
      }
      else false
    }
  

}
