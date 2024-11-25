package week01

object week02_project:
    type Set = Int => Boolean
    def contains(elem:Int, s: Set) = s(elem)
    def singleton(elem:Int): Set = _ == elem
    def union(s0: Set, s1: Set): Set = x => s0(x) || s1(x)
    def intersection (s0: Set, s1: Set): Set = x => s0(x) && s1(x)
    def diff(s0: Set, s1: Set): Set = x => s0(x) && (!s1(x))
    def filter(s: Set, f: Int => Boolean): Set = intersection(s, f)
    def forall(s: Set, f: Int => Boolean): Boolean = 
        def test_within(a:Int,b:Int):Boolean = 
            if a > b then test_within(b,a)
            else if a == b then 
                if s(a) then f(a) else true
            else if s(a) then 
                if f(a) then test_within(a+1,b) else false
            else test_within(a+1,b)

        test_within(-1000,1000)

    def exists(s: Set, f: Int => Boolean): Boolean = !forall(s, x => !f(x))
    def map(s: Set, f: Int => Int): Set = 
        def char_func(x: Int) : Boolean = 
            exists(s, t => f(t) == x )
        char_func