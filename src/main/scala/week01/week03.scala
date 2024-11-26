package week01
import math.max
@main def week03() = 
    var t:IntSet = E
    t = t.include(3)
    t = t.include(5)
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
    
    

case object E extends IntSet:
    def contains(elem: Int): Boolean = false
    def include(elem: Int): Node = Node(elem, E,E)
    override def toString():String = "E"
    def prettyPrint = List(" E ")


case class Node(x: Int, left: IntSet, right:IntSet) extends IntSet:
    def contains(elem: Int): Boolean = x == elem || left.contains(elem) || right.contains(elem)
    def include(elem: Int): Node = 
        if elem == x then this
        else if elem > x then right.include(elem)
            else left.include(elem)

    def rotateLeft: IntSet = 
        right match
            case E => this
            case r:Node =>
                val new_left = Node(x, left, r.left)
                Node(r.x, new_left, r.right)
    def rotateRight: IntSet =
        left match
            case E => this
            case l:Node =>
                val new_right = Node(x, l.right, right)
                Node(l.x, l.left, new_right)

    def rotateRightLeft: IntSet =
        right match
            case E => this
            case r : Node => 
                val new_right = r.rotateRight
                Node(x, left, new_right).rotateLeft

    def rotateLeftRight: IntSet =
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