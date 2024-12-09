package week04

@main def test04 = 
    println(Zero)
    val three = Succ(Succ(Succ(Zero)))
    val four = Succ(three)
    println(s"$three + $four = ${three + four}")
    println(s"$three * $four = ${three * four}")

    val c3 = MyInt(3)
    val c4 = MyInt(4)
    val s = Sum(c3, c4)
    val p = Prod(c3 ,c4)
    println(s"$s = ${s.eval.value}")
    println(s"$p = ${p.eval.value}")
    val p2 = Prod(s, p)
    println(s"$p2 = ${p2.eval.value}")

def insertionSort(t:Int, ts:List[Int]): List[Int] = ts match
    case head :: next => if t > head then head :: insertionSort(t, next) else t::ts
    case Nil => t :: Nil




sealed trait Nat:
    def isZero: Boolean
    def pred: Nat
    def succ: Nat
    infix def + (that: Nat): Nat
    infix def - (that: Nat): Nat
    infix def *(that:Nat): Nat


case object Zero extends Nat:

    def isZero: Boolean = true
    def pred: Nat = Zero
    def succ: Nat = Succ(Zero)
    infix def + (that: Nat): Nat = that
    infix def - (that: Nat): Nat = this
    infix def *(that:Nat): Nat = this
    override def toString(): String = "0"


case class Succ(val pred: Nat) extends Nat:
    def isZero: Boolean = false
    def succ: Nat = Succ(this)
    infix def + (that: Nat): Nat = if that.isZero then this else pred + Succ(that)
    infix def - (that: Nat): Nat = that match
        case Zero => this
        case Succ(opred) => pred - opred
    infix def *(that:Nat): Nat = that match
        case Zero => Zero
        case Succ(Zero) => this
        case Succ(opred) => (this * opred) + this
    
    override def toString(): String = (pred.toString().toInt + 1).toString()


sealed trait Expr[T]:
    def eval: Const[T]
    def substitute(subst: Expr[T] => Expr[T]): Expr[T]

class Const[T](const : T) extends Expr[T]:
    def value = const
    def eval = this
    def substitute(subst: Expr[T] => Expr[T]):  Expr[T] = this
    override def toString(): String = value.toString()

class Var[T](name: String) extends Expr[T]:
    def eval = throw Exception("Variable cannot be evaluated")
    def substitute(subst: Expr[T] => Expr[T]): Expr[T] = subst(this)
    override def toString(): String = "?"+name
    
trait BinaryOp[T](left: Expr[T], right:Expr[T]) extends Expr[T]


case class MyInt(val const:Int) extends Const[Int](const)
case class Sum(left:Expr[Int], right: Expr[Int]) extends BinaryOp[Int](left, right):
    def eval: MyInt = MyInt(left.eval.value + right.eval.value)
    def substitute(subst: Expr[Int] => Expr[Int]): Expr[Int] = 
        Sum(left.substitute(subst), right.substitute(subst))
    override def toString(): String = left.toString() +" + "+right.toString()

case class Prod(left:Expr[Int], right:Expr[Int]) extends BinaryOp[Int](left,right):
    def substitute(subst: Expr[Int] => Expr[Int]): Expr[Int] = 
        Prod(left.substitute(subst), right.substitute(subst))
    def eval: MyInt = MyInt(left.eval.value * right.eval.value)
    override def toString(): String = 
        val left_str  = left match
            case x:Sum => s"(${x.toString()})"
            case x => x.toString()

        val right_str = right match
            case x: Sum => s"(${x.toString()})"
            case x => x.toString()
        s"$left_str * $right_str"

    