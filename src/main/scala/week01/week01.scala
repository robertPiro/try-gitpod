package week01

object Week01 {
    def main(args:Array[String]): Unit = 
        val sqrt_2 = newton_raphson(x=> x*x-2, x=> 2*x, 3)
        println(sqrt_2)


    def newton_raphson(f:Double => Double, fprime: Double => Double, start_value:Double, iter:Int =1000):Double =
        if iter <= 0
        then start_value
        else 
            val x = start_value - (f(start_value)/fprime(start_value))
            newton_raphson(f, fprime, x, iter -1)

    
}
