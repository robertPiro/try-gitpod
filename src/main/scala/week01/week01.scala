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



}
