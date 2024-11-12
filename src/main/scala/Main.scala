object Main:
  def main(args:Array[String]):Unit =
    println("Hello World!")
    println(max(List(3,1,2)))
    println(and(true, false))
    printPascal(10)

  def max(l:List[Int]): Int = 
    if l.isEmpty then throw Exception("List is empty. Max undefined")
    
    def rec_max(max_elem:Int, l:List[Int]):Int = 
      l match
        case Nil => max_elem
        case head :: tail => 
          if head > max_elem 
          then rec_max(head, tail) 
          else rec_max(max_elem, tail)
    
    rec_max(l.head, l.tail)

  def and(x:Boolean, y: => Boolean): Boolean = if x then y else false

  def pascal(row: Int, col: Int):Int =
    if row == col then 1
    else
        if col== 0 then 1
        else
            pascal(row-1, col-1) + pascal(row-1, col)


  def printPascal(rows: Int)=
    for row <- 0 to rows
        col <- 0 to row
    do
        print(pascal(row,col))
        print("  ")
        if row == col then println()