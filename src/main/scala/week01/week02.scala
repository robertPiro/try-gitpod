package week01

@main def week02(): Unit =
    println(sum( (x:Int) => x, 1,2))
    println(prod(x => x)(1,5))
    // sqrt(x) is a fixed point of f(x) => x/f(x)
    println(sqrt(2))
    
def sqrt(x:Double) = 
    fixedpoint( y => x/y )(start_value = 1, tolerance = 0.0001)


def sum(f: Int => Int, a: Int, b: Int): Int = 
    def loop(a: Int, acc: Int): Int = 
        if a > b then acc 
        else loop(a+1, acc + f(a))
    loop(a, 0)

def prod(f: Int => Int)(a:Int, b:Int)=
    def rec(a: Int, acc: Int): Int =
        if a > b then acc
        else rec(a+1, acc * f(a))
    rec(a, 1)

def dampener(f: Double => Double): Double => Double =
    x => (x + f(x))/2  // It prevents the search for a fixed point function to oscillate

def fixedpoint(f: Double => Double)(start_value:Double, tolerance: Double) =
    def abs(x:Double) = if x >=0 then x else -x
    def good_enough(value: Double, next_value: Double) =
        if value == 0 then abs(next_value) < tolerance 
        else abs((value - next_value)/value)/value < tolerance
    def g = dampener(f)
    def rec(value:Double): Double =
        val next_value = g(value)
        if good_enough(value, next_value) then next_value
        else rec(next_value)
    rec(start_value)


