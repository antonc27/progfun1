package recfun

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
  def pascal(c: Int, r: Int): Int = {
    if (c < 0 || c > r) {
      0
    } else {
      if (c == 0 || c == r) {
        1
      } else {
        pascal(c-1, r-1) + pascal(c, r-1)
      }
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def loop(acc: Int, chars: List[Char]): Boolean = {
      if (acc < 0) {
        return false
      }

      if (chars.isEmpty) {
        return acc == 0
      }

      val next = chars.head
      if (next == '(') {
        loop(acc+1, chars.tail)
      } else {
        if (next == ')') {
          loop(acc-1, chars.tail)
        } else {
          loop(acc, chars.tail)
        }
      }
    }

    loop(0, chars)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0) {
      return 0
    }

    if (money > 0 && coins.isEmpty) {
      return 0
    }

    if (money == 0 && coins.isEmpty) {
      return 1
    }

    countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
