package week01
import math.abs
extension (t: Int) infix def * (other: Rational): Rational = other * t
extension (t: Int) infix def $ (other: Int): Rational = Rational(t, other)

@main def week02(): Unit =
    println(sum( (x:Int) => x, 1,2))
    println(prod(x => x)(1,5))
    // sqrt(x) is a fixed point of f(x) => x/f(x)
    println(sqrt(2))
    println(s"${3 $ 4} + ${1 $ 10} = ${(3 $ 4) + (1 $ 10)}")
    println(5 * (Rational(1,2)))
    
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
        else abs((value - next_value)/value) < tolerance
    def g = dampener(f)
    def rec(value:Double): Double =
        val next_value = g(value)
        if good_enough(value, next_value) then next_value
        else rec(next_value)
    rec(start_value)


object Rational:
    def gcd(x: Int, y:Int): Int =
        if y == 0 then x
        else gcd(y, x % y) 


class Rational(val num: Int, val denom:Int):
    override def toString(): String = s"(${num}/${denom})"
    
    val rationalized: Rational = 
        if denom == 0 then throw Exception("Rational instantiated with denominator 0.")
        val gcd = Rational.gcd(abs(num), abs(denom))
        if gcd == 1 then 
            if denom < 0 then Rational(-1* num, -1*denom) else this 
        else Rational(num/gcd, denom/gcd)
    
    lazy val inverse = Rational(rationalized.denom, rationalized.num)
    
    infix def * (int: Int):Rational = 
        this * Rational(int,1)

    infix def * (other: Rational): Rational =
        val a = rationalized
        val b = other.rationalized
        Rational(a.num * b.num, a.denom * b.denom)

    infix def /(other: Rational): Rational =
        this * other.inverse  

    infix def + (other: Rational): Rational = 
        val (a, b) = extendWith(other)
        Rational(a.num + b.num, a.denom)

    def extendWith(other:Rational): (Rational, Rational) = 
        val a = rationalized
        val b = other.rationalized
        if a.denom == b.denom then (a,b)
        else
            val gcd = Rational.gcd(a.denom,b.denom)
            val extendB = a.denom / gcd
            val extendA = b.denom / gcd
            val new_denom = a.denom * extendA
            (Rational(a.num * extendA, new_denom), Rational(b.num * extendB, new_denom))

    
    infix def - (other:Rational) =
        this + (other * -1)
        
        



    
