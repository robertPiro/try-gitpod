package week01
import math.max
@main def main() = {}

sealed trait IntSet:
    def contains(elem: Int): Boolean
    def include(elem: Int): Node
    def height: Int = this match
        case E => 1
        case n:Node => max(n.left.height, n.right.height) + 1
    def balancingFactor: Int = this match
        case E => 0
        case n: Node => n.right.height - n.left.height
    
    

case object E extends IntSet:
    def contains(elem: Int): Boolean = false
    def include(elem: Int): Node = Node(elem, E,E)


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
