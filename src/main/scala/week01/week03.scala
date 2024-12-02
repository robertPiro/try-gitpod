package week01
import math.max

@main def week03() = 
    var t:IntSet = E
    t = t.include(3)
    t = t.include(5)
    t = t.include(4)
    println(t.prettyPrint.mkString("\n"))
    t = t.balance
    println("=======================")
    println(t.prettyPrint.mkString("\n"))

sealed trait IntSet:
    def contains(elem: Int): Boolean
    def include(elem: Int): Node
    def height: Int = this match
        case E => 1
        case n:Node => max(n.left.height, n.right.height) + 1
    def balancingFactor: Int = this match
        case E => 0
        case n: Node => n.right.height - n.left.height
    def prettyPrint:List[String]
    def balance: IntSet
    def depthFirst: List[Int]

    
    

case object E extends IntSet:
    def contains(elem: Int): Boolean = false
    def include(elem: Int): Node = Node(elem, this, this)
    override def toString():String = "E"
    def prettyPrint = List(" E ")
    def balance = this
    def depthFirst = List()



case class Node(x: Int, left: IntSet, right:IntSet) extends IntSet:
    def contains(elem: Int): Boolean = x == elem || left.contains(elem) || right.contains(elem)
    def include(elem: Int): Node = 
        if elem == x then this
        else if elem > x then Node(x, left, right.include(elem)).balance
            else Node(x, left.include(elem), right).balance
    def depthFirst = (left.depthFirst :+ x) ++ right.depthFirst
    def breadthFirst = 
        def rec(ints:List[Int], nodes:List[IntSet]): List[Int] =
            nodes match
                case Nil => ints
                case head::tail => 
                    head match 
                        case E => rec(ints, tail)
                        case Node(x, l, r) => rec(ints :+ x, nodes :+ l :+ r)
        rec(List(), List(this))

    def union(that: IntSet): IntSet =
        that.depthFirst.foldLeft(this)( (acc, int) => acc.include(int))
    def balance: Node =
        balancingFactor match
            case 2 => right.balancingFactor match
                case -1 => this.rotateRightLeft
                case _ =>  this.rotateLeft
            case -2 => left.balancingFactor match
                case 1 => this.rotateLeftRight
                case _ => this.rotateRight
            case _ => this
            
    def rotateLeft: Node = 
        right match
            case E => this
            case r:Node =>
                val new_left = Node(x, left, r.left)
                Node(r.x, new_left, r.right)
    def rotateRight: Node =
        left match
            case E => this
            case l:Node =>
                val new_right = Node(x, l.right, right)
                Node(l.x, l.left, new_right)

    def rotateRightLeft: Node =
        right match
            case E => this
            case r : Node => 
                val new_right = r.rotateRight
                Node(x, left, new_right).rotateLeft

    def rotateLeftRight: Node =
        left match
            case E => this
            case l : Node => 
                val new_left = l.rotateLeft
                Node(x, new_left, right).rotateRight



    def prettyPrint: List[String] =
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