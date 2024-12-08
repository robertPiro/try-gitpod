import math.max
import math.Ordering
@main def test()=
    var t: MySet[Int] = E
    t = t.include(3)
    t = t.include(5)
    t = t.include(4)
    println(t.prettyPrint.mkString("\n"))
    println("=======================")
    var s: MySet[Int] = E
    s = s.include(3)
    s = s.include(5)
    s = s.include(7)
    println(s.prettyPrint.mkString("\n"))



sealed trait MySet[+T]:
    def contains[S >: T](elem: S)(using ord:Ordering[S]): Boolean
    def include[S >: T](elem: S)(using ord:Ordering[S]): MySet[S]
    def height: Int = this match
        case E => 0
        case _: Singleton[T] => 1 
        case n: Node[T] => max(n.left.height, n.right.height) + 1

    def balancingFactor: Int = this match
        case E => 0
        case _: Singleton[T] => 0
        case n: Node[T] => n.right.height - n.left.height

    def prettyPrint:List[String] = List(s" ${toString()} ")
    def balance: MySet[T]
    def depthFirst: List[T]
    def breadthFirst: List[T]

case object E extends MySet[Nothing]:
    def contains[S >: Nothing](elem: S)(using ord:Ordering[S]): Boolean = false
    def include[S >: Nothing](elem: S)(using ord:Ordering[S]): Singleton[S] = Singleton(elem)
    override def toString():String = "E"
    def balance = this
    def depthFirst = List()
    def breadthFirst = List()

case class Singleton[+T] (val x:T) extends MySet[T]:
    def contains[S >: T](elem: S)(using ord:Ordering[S]): Boolean = x == elem
    def include[S >: T](elem: S)(using ord:Ordering[S]): MySet[S] = 
        if elem == x then this
        else if ord.gt(elem,x) then Node(x, E, Singleton(elem))
        else Node(x, Singleton(elem), E)

    override def toString():String = x.toString()
    def balance = this
    def depthFirst = List(x)
    def breadthFirst = List(x)

case class Node[+T](val x: T, left: MySet[T], right:MySet[T]) extends MySet[T]:
    def contains[S >: T](elem: S)(using ord:Ordering[S]): Boolean = 
        if x == elem then true
        else if ord.lt(elem,x) then left.contains(elem)
        else right.contains(elem)

    def include[S >: T](elem: S)(using ord:Ordering[S]): Node[S] = 
           if elem == x then this
           else if ord.gt(elem,x) then Node(x, left, right.include(elem)).balance
           else Node(x, left.include(elem), right).balance

    def depthFirst: List[T] = (left.depthFirst :+ x) ++ right.depthFirst
    def breadthFirst: List[T] = 
        def rec(elems:List[T], nodes:List[MySet[T]]): List[T] =
            nodes match
                case Nil => elems
                case head::tail => 
                    head match 
                        case E => rec(elems, tail)
                        case Singleton(x) => rec(elems :+ x, tail)
                        case Node(x, l, r) => rec(elems :+ x, nodes :+ l :+ r)
        rec(List(), List(this))

    def union[S >: T](that: MySet[S])(using Ordering[S]): MySet[S] =
        that.depthFirst.foldLeft(this)( (acc:MySet[S], int:S) => acc.include(int))
    def balance: Node[T] =
        balancingFactor match
            case 2 => right.balancingFactor match
                case -1 => this.rotateRightLeft
                case _ => this.rotateLeft
            case -2 => left.balancingFactor match
                case 1 => this.rotateLeftRight
                case _ => this.rotateRight
            case _ => this

    def rotateLeft: Node[T] = 
        right match
            case r: Node[T] =>
                val new_left = (left, r.left) match
                    case (E, E) => Singleton(x)
                    case _ => Node(x, left, r.left)
                Node(r.x, new_left, r.right)
            case Singleton(l) => Node(l, Singleton(x), E)
            case _ => this
    def rotateRight: Node[T] =
        left match
            case l: Node[T] =>
                val new_right = (l.right, right) match
                    case (E,E) => Singleton(x)
                    case _ => Node(x, l.right, right)
                Node(l.x, l.left, new_right)
            // subtree must have balancing factor of -1 or 0
            // if tree is -1 then right is E and rotation moves root into singleton
            // if tree is 0 the right is singleton and tree was unbalanced when
            // elem of left or elem of right was inserted. this is in contradiction 
            // to the properties of AVL trees.
            case Singleton(l) => Node(l, E, Singleton(x))
            case _ => 
                this

    def rotateRightLeft: Node[T] =
        right match
            case r : Node[T] => 
                val new_right = r.rotateRight
                println("================== New Right")
                println(new_right.prettyPrint.mkString("\n"))
                println("===========================")
                Node(x, left, new_right).rotateLeft
            case _ => this

    def rotateLeftRight: Node[T] =
        left match
            case l : Node[T] => 
                val new_left = l.rotateLeft
                Node(x, new_left, right).rotateRight
            case _ => this



    override def prettyPrint: List[String] =
        // invariant: every line has the same width
        // every line has an odd width
        var ls = left.prettyPrint // List cannot be empty
        var rs = right.prettyPrint
        val lsize = ls.head.size 
        val rsize = rs.head.size

        // making lists same length and combine
        if ls.size < rs.size 
        then ls = (0 to rs.size).foldLeft(ls): 
            (acc, i) => if i >= ls.size then acc :+ " "*lsize else acc
        else rs = (0 to ls.size).foldLeft(rs):
            (acc, i) => if i >= rs.size then acc :+ " "*rsize else acc

        var subtree = ls.zip(rs).map( (l, r) => l + " " + r)
        val lhb = (" " * (lsize/2)) + ("-"* (lsize-lsize/2))  // same size as lsize
        val rhb = ("-" * (rsize-rsize/2)) + (" " * (rsize/2)) // same size as rsize
        subtree = (lhb + '+' + rhb) +: subtree

        var n = " " + x.toString() + " "
        n = if n.size %2 ==0 then n +" " else n

        val topl = " " *(lsize - n.size/2) + n + " "*(rsize - n.size/2)
        topl +: subtree