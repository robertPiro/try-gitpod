package week01

object Week01 {
    def main(args:Array[String]): Unit = 
        val sqrt_2 = newton_raphson(x=> x*x-2, x=> 2*x, 3)
        println(sqrt_2)
        println(gcd(15, 27))
        val l = List(1,2,3,4,5)
        println(s"""${l} apply fac ${l.map(fac)} """ )
        loop(3):
            x => println("*"*x)
        printPascal(6)
        val balances = Seq("(if (zero? x) max (/ 1 x))", "I told him (that it’s not (yet) done). (But he wasn’t listening)",
        ":-)", "())(" )
        balances.foreach(x => println(s"$x is ${if balance(x) then "" else "not"} balanced "))

        val denom = List(1,2,5,10,20,50)
        print(countChange(4, denom))

    def newton_raphson(f:Double => Double, fprime: Double => Double, start_value:Double, iter:Int =1000):Double =
        if iter <= 0
        then start_value
        else 
            val x = start_value - (f(start_value)/fprime(start_value))
            newton_raphson(f, fprime, x, iter -1)

    def gcd(x:Int, y:Int):Int =
        if y == 0 then x else gcd(y, x%y)

    def fac(x:Int): Int =
        if x <= 0
        then 1
        else x * fac(x-1)
    
    def ackermann(x:Int, y:Int, op:Int): Int = ???
        
    def loop(n:Int)(f: Int => Unit) =
        def rec(k:Int):Unit =
            if k > 0 
            then 
                f(k)
                rec(k-1)
        rec(n)

    def pascal(row: Int, col: Int):Int =
      if row == col then 1
      else
          if col== 0 then 1
          else
              pascal(row-1, col-1) + pascal(row-1, col)


    def printPascal(rows: Int)=
      for row <- 0 to rows
          col <- 0 to row
      do
          print(pascal(row,col))
          print("  ")
          if row == col then println()


    def balance(chars: String): Boolean =
        def rec(chars:List[Char], openPar:Int=0): Boolean =
            if chars.isEmpty then openPar == 0
            else chars.head match
                case '(' => rec(chars.tail, openPar + 1)
                case ')' => openPar > 0 && rec(chars.tail, openPar - 1)
                case _ => rec(chars.tail, openPar)
        rec(chars.toList)

    def countChange(money: Int, coins: List[Int]): Int = {
        def rec (money:Int, coins:List[Int]): List[List[Int]] =
            if money == 0 then List(Nil)
            else 
                if coins.isEmpty then Nil
                else
                    val coin = coins.head
                    if coin > money then Nil
                    else 
                        val res = rec(money - coin, coins)
                        res.map(x => coin :: x) ++ rec(money, coins.tail)
        val uniqueCoins = coins.toSet.filter(x => x > 0).toList.sorted
        val res = rec(money, uniqueCoins)
        res.size
    }
}
