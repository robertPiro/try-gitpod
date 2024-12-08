package week04

import scala.collection.mutable.HashMap

@main def project_week04 = {}
  

trait HuffmanTree:
    def leafs(s:String): List[HuffmanLeaf] = leafs(s.toCharArray())
    def leafs(cs:Array[Char]): List[HuffmanLeaf] = 
        val hmap = HashMap[Char, Int]()
        cs.foldLeft(hmap)((acc,char) => 
            acc.updateWith(char):
                case Some(count) => Some(count + 1)
                case None => Some(1)
        )
        hmap.toList.map( (char,count) => HuffmanLeaf(char, count))


case class HuffmanNode(zero: HuffmanTree, one: HuffmanTree) 
case class HuffmanLeaf(char: Char, freq:Int) extends HuffmanTree

