

object  Main {
  def main(args: Array[String]) {
    /** *
      * ********************* Test Pascal's Traingle Exercise ***********************
      */
    println("Pascal's Triangle")
    val a = scala.io.StdIn.readInt()
    for (row <- 0 to a) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    /** *
      * ********************* Test Balance Exercise ***********************
      */
    var paranthesis = "!254))(((54?" //The String
    println(balance(paranthesis.toList))



  }

  /** *
    * ********************* Pascal's Traingle Exercise ***********************
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == r || c <= 0) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }


  /** *
    * ********************************** Balance Exercise ************************
    */


  def balance(chars: List[Char]): Boolean = {
    def balances(chars: List[Char], num: Int): Boolean = {
      (chars,num) match {
        case (Nil,num) => return num == 0
        case (c :: tail,num) =>
          if(c == '(') return balances(tail,num+1)
          if(c == ')') return balances(tail,num-1)
          else return balances(tail,num)
      }
    }
    balances(chars,0)
  }




  /** *
    * ********************************** Counting Change Exercise************************
    */


  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty) 0  
    else if (money - coins.head == 0) 1
    else if (money - coins.head < 0) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }



}
