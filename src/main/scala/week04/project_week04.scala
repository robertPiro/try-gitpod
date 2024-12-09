package week04

import scala.collection.mutable.HashMap
import scala.collection.mutable.Queue

@main def project_week04 = {}
  

trait HuffmanTree(val weight:Int):
    def leafs(s:String): List[HuffmanLeaf] = leafs(s.toCharArray())
    def leafs(cs:Array[Char]): List[HuffmanLeaf] = 
        val hmap = HashMap[Char, Int]()
        cs.foldLeft(hmap)((acc,char) => 
            acc.updateWith(char):
                case Some(count) => Some(count + 1)
                case None => Some(1)
            acc
        )
        hmap
            .toList
            .map( (char,count) => HuffmanLeaf(char, count) )
            .sortWith((a,b) => a.weight < b.weight)
    
    def buildTree(ascLeaves:List[HuffmanLeaf]): HuffmanTree =
        def pullSmallest(ascLeaves:List[HuffmanLeaf], nodeQ:Queue[HuffmanTree]): (HuffmanTree, List[HuffmanLeaf], Queue[HuffmanTree]) = 
            ascLeaves match
                case head :: next => nodeQ match
                    case n +: queue => 
                        if head.weight < n.weight then (head, next, nodeQ)
                        else (n, ascLeaves, queue)
                    case _ => (head, next, nodeQ)
                case Nil => (nodeQ.head, Nil, nodeQ.tail)
            
        def rec(ascLeaves:List[HuffmanLeaf], nodeQ:Queue[HuffmanTree]): HuffmanTree = 
            // requires that at least one collection is non-empty
            val (low0, newLeaves0, newQueue0) = pullSmallest(ascLeaves, nodeQ)
            (newLeaves0, newQueue0) match
                case (Nil, Queue()) => low0
                case _ => 
                    val (low1, newLeaves1, newQueue1) = pullSmallest(newLeaves0, newQueue0)
                    val newNode = HuffmanNode(low0, low1)
                    rec(newLeaves1, newQueue1 :+ newNode) // ensures that at least Queue is non-empty

            
                 
        ascLeaves match
            case Nil => throw Exception("List of leaves is empty. Cannot compute Huffman tree.")
            case _ => rec(ascLeaves, Queue())

case class HuffmanNode(zero: HuffmanTree, one: HuffmanTree) extends HuffmanTree(zero.weight + one.weight)
case class HuffmanLeaf(char: Char, weight:Int) extends HuffmanTree(weight)

/**** The original instruction for the Huffman algorithm was as follows:
 *    0. create the List of ordered leaves then
 *    1. combine the first two elements in a node and push this new
 *    2. node down the list until the list is ordered again (insertion sort)
 *    3. repeat 1 + 2 until only one node is left. That is the root to the HuffmanTree.
 *
 *    While the insertion is possible, it creates a lot of packing and unpacking and operations that
 *    are quadratic in nature. Here we implement the algorithm with a queue, to which we append the created
 *    nodes. We always take the two minimal elements from the head-elements of the ordered List L of
 *    leaves and the Queue Q (if they exists. See pull smallest.)
 * 
 *    To make the algorithm work, we need to guarantee that we only combine the leaves and nodes with the 
 *    minimal weights. That can only work if not only L is ordered but also Q. But the latter is not immediately
 *    obvious. L is ordered in the pre-step, the same way it is ordered in step 0. above.
 * 
 *    The prove that for every step in the algorithm the following conditions hold:
 *    a) Q is ordered (ascendingly from the head)
 *    b) the last element added is greater than all elements in Q and the sum of the minimal head elements.
 *    The proof is by induction on the number of steps of recursive calls of rec:
 *
 *    Base Case: In step 0, Q is empty and thus trivially ordered. Additionally, the two head elements l0 and l1 
 *    of L are minimal because L is ordered and thus for all m in Q (which is empty) we have m <= l0 + l1.
 * 
 *    Induction step: Let n be an arbitrary step. Since we add an element in each step, we can assume n > 0 
 *    and Q is non-empty. According to the induction hypothesis (IH), Q is ascendingly ordered and the last 
 *    element in Q is the sum of two minimal elements x,y among the head elemnts of Q and L.
 * 
 *    Note that if only one element exists, the algorithm stops. Let w.o.l.g. x <= y and assume, equally w.o.l.g.
 *    that Q = q1 :: q2 :: Q' and L = l1 :: l2 :: L'. Then, x,y <= l1, l2 because x,y were chosen minimally and L
 *    is ordered by assumption. Similarly x,y <= q1, q2, because Q is ordered and the elements were chosen
 *    minimally.
 * 
 *    It thus follows that for z := min{ l1 + l2, l1 + q1, q1 + q2}, x+y <= z. Hence Q :+ z is again ordered, 
 *    z a maximal element in Q, and z is the sum of the two minimal head elements of Q and L. QED
 *    
 *   
 * /