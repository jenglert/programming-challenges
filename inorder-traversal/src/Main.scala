

import scala.collection.mutable.Stack
import scala.math._

object TreeReader {
  
  case class LineValue(value: String, lineNum: Int, spaceNumber: Int, foundNode: Option[Node[String]], var processedFlag: Boolean = false)
  
  def readFromString(str: String): Node[String] = {
     val lines = str.split("\n").filter(!_.replaceAll("[/\\\\]", "").trim().isEmpty)
     
     val lineValues = lines.zipWithIndex.flatMap(p => readLine(p._1, p._2))
     val lastLineNumber = lineValues.map(_.lineNum).max
     
     // Create leaf nodes for the bottom row.
     val bottomLineValues = lineValues.filter(_.lineNum == lastLineNumber).map { lv => 
       lv.copy(foundNode = Some(Node[String](None, None, lv.value)))
     }

     val results = (0 to (lastLineNumber - 1)).reverse.foldLeft[Seq[LineValue]](bottomLineValues) { (foundNodes, i) => 
       matchLinesToNodes(lineValues.filter(_.lineNum == i), foundNodes)
     }
     
     results.find(_.lineNum == 0).get.foundNode.get // So much unchecked bullshit
  }

  /**
   * Matches nodes from the previous line to line values from the next line up.
   */
  def matchLinesToNodes(lineNodes: Seq[LineValue], foundNodes: Seq[LineValue]): Seq[LineValue] = {
    assert(lineNodes.map(_.lineNum).toSet.size == 1)
    val processingLineNumber = lineNodes.head.lineNum
    
    var result = foundNodes
    
    // Children will be one line down.
    val candidateNodes = foundNodes.filter(_.lineNum == processingLineNumber + 1).sortBy(_.spaceNumber)
    
    matchLines(candidateNodes, lineNodes).groupBy(_._2).foreach { case(top, bottoms) =>
      var left: Option[Node[String]] = None
      var right: Option[Node[String]]  = None
      
      bottoms.map(_._1).foreach { b => 
        
        if (top.spaceNumber > b.spaceNumber) {
          left = b.foundNode
        }
        
        if (top.spaceNumber < b.spaceNumber) {
          right = b.foundNode
        }
      }
      
      result :+= top.copy(foundNode = Some(Node(left, right, top.value)))
    }
    
    // Add new leaf nodes
    lineNodes.filter(ln => !result.map(r => (r.lineNum, r.spaceNumber)).contains(ln.lineNum, ln.spaceNumber)).foreach { ln =>
      result :+= ln.copy(foundNode = Some(Node(None, None, ln.value)))
    }
    
    result
  }

  // matches
  def matchLines(bottomLines: Seq[LineValue], topLines: Seq[LineValue]): Seq[Tuple2[LineValue, LineValue]] = {
    case class LineMatch(bottomLine: LineValue, topLine: LineValue, distance: Int)
    
    val matches: Seq[LineMatch] = bottomLines.flatMap { b => 
      topLines.map { t => 
        LineMatch(b, t, abs(b.spaceNumber - t.spaceNumber))  
      }  
    }
    
    matches.groupBy(_.bottomLine).map { case (blv, tls) =>
      (blv, tls.sortBy(_.distance).head.topLine)
    }.toList
  }
  
  def readLine(str: String, lineNum: Int): Seq[LineValue] = {
     str.toCharArray.zipWithIndex.flatMap { case (ch, i) =>
       if (ch.toString.trim.isEmpty) {
         // Whitespace
         None
       } else { 
         Some(new LineValue(ch.toString, lineNum, i, None))
       }
     }
  }
}

case class Node[T](left: Option[Node[T]], right: Option[Node[T]], value: T) {
  override def toString = value.toString
  
  import Node._
  
  def printTree = { 
    val drawWidth: Int = (pow(2, maxDepth + 1) + 1).toInt
        
    val printInstructions = printTreeInternal(this.toStringNode, 0, (drawWidth / 2).toInt, maxDepth)
    
    def printSpace(times: Int) = (1 to times).foreach { i => print(" ") }
    
    def drawTopBottom = println((1 to (drawWidth + 2)).map(_ => "*").mkString)
    
    drawTopBottom
    printInstructions.groupBy(_.line).toList.sortBy(_._1).foreach { case (line, lineValues) =>
      print("*")
      var currentSpace = 0
      
      lineValues.sortBy(_.space).foreach { lineValue => 
        printSpace(lineValue.space - currentSpace)
        print(lineValue.value)
        currentSpace = lineValue.space + 1
      }
      
      printSpace(drawWidth - currentSpace)
      
      println("*")
    }
    drawTopBottom    
  }
  
  def toStringNode: Node[String] = new Node[String](left.map(_.toStringNode), right.map(_.toStringNode), value.toString)
  
  // The maximum width of the tree (counting non-present nodes)
  def maxDepth: Int = {
    max(left.map(_.maxDepth).getOrElse(0), right.map(_.maxDepth).getOrElse(0)) + 1
  }
}

case class PrintInstruction[T](value: T, line: Int, space: Int)

object Node { 
  def printTreeInternal(node: Node[String], line: Int, space: Int, maxDepth: Int): List[PrintInstruction[String]] = {
    var result: List[PrintInstruction[String]] = List(PrintInstruction(node.value, line, space))
    
    if (node.left.isDefined) {
       result = result :+ PrintInstruction("/", line + 1, (space - pow(2, maxDepth - (line / 2) - 1) / 2).toInt)
    }
    
    if (node.right.isDefined) { 
      result = result :+ PrintInstruction("\\", line + 1, (space + pow(2, maxDepth - (line / 2) - 1) / 2).toInt)
    }
    
    val leftTree: List[PrintInstruction[String]] = node.left.toList.flatMap(n => 
      printTreeInternal(n, line + 2, space - pow(2, maxDepth - (line / 2).toInt - 1).toInt, maxDepth)
    )
    val rightTree: List[PrintInstruction[String]] = node.right.toList.flatMap(n => 
      printTreeInternal(n, line + 2, space + pow(2, maxDepth - (line / 2).toInt - 1).toInt, maxDepth)
    )
    
    (result ++ leftTree) ++ rightTree
  }
}

class InOrderIterator[T](root: Node[T]) extends Iterator[T] {
  
  private val trail = Stack.empty[Node[T]]
  
  private var lastNode: Option[Node[T]] = None
  
  private var done = false
  
  private var currentNode = root 
  downLeft
  
  // Algorithm:
  //   Start by going all the way down to the left.
  //   On each node:
  //     1. return value
  //     2. if has right and didn't come from right, move down-right
  //     3. if has left and didn't come from left or right, move down left (recursively)
  //     4. move to parent
  //
  //   Done when you get to a node
  
  override def hasNext = !done

  override def next(): T = {

    val ret = currentNode.value
    
    currentNode match { 
      // Process the left side
      case node if node.left.isDefined && !cameFromLeft && !cameFromRight =>
        downLeft
      // Process the right side.
      case node if node.right.isDefined && !cameFromRight =>
        lastNode = Some(currentNode)
        trail.push(currentNode)
        currentNode = node.right.get
        downLeft
      // Coming from the left side
      case node if downToLeft =>
        upOne
      case node if downToRight =>
        upRight
        upOne
      case node if (cameFromLeft && node.right.isEmpty) || cameFromRight => 
        upOne
    }
    
    ret
  }
  
  private def upOne { 
    if (!trail.isEmpty) {
      lastNode = Some(currentNode)
      currentNode = trail.pop
    } else { 
      done = true
    }
  }
  
  // We keep going up right as long as we came from the right.
  private def upRight {
    if (downToRight && !trail.isEmpty) {
      lastNode = Some(currentNode)
      currentNode = trail.pop
      upRight
    }
  }
  
  private def downLeft {
    if (currentNode.left.isDefined) { 
      trail.push(currentNode) 
      lastNode = Some(currentNode)
      currentNode = currentNode.left.get
      downLeft
    }
  }
  
  private def cameFromRight = lastNode.map(_ == currentNode.right.orNull).getOrElse(false) 
  private def cameFromLeft = lastNode.map(_ == currentNode.left.orNull).getOrElse(false)
  
  private def downToRight = trail.headOption.map(_.right.orNull == currentNode).getOrElse(false)
  private def downToLeft = trail.headOption.map(_.left.orNull == currentNode).getOrElse(false)
}

object Main extends App {
  import TestTrees._, TreeReader._
  
  def printInOrder(tree: Node[String]) {
    print("In Order: ")
    new InOrderIterator(tree).take(20).foreach(print)
    println("")  
  }

  val simpleRoot = readFromString(Simple)
  simpleRoot.printTree
  printInOrder(simpleRoot)
  
  val mediumRoot = readFromString(MediumTree)
  mediumRoot.printTree
  printInOrder(mediumRoot)
  
  val hardRoot = readFromString(HardTree)
  hardRoot.printTree
  printInOrder(hardRoot)
  
  val leftRoot = readFromString(ToTheLeft)
  leftRoot.printTree
  printInOrder(leftRoot)
  
  val rigthRoot = readFromString(ToTheRight)
  rigthRoot.printTree
  printInOrder(rigthRoot)
}


// Test trees
object TestTrees { 
val Simple = """
     b 
    / \  
   a   c   
"""
  
val MediumTree = """
            c
        /       \
        b       e
      /   \   /    \
      a       d     f
"""

val HardTree = """
                        e
                            
                  c          f
                              
             b        d           g
           
          a                          h
"""

val ToTheLeft = """
a
 b
  c
   d
    e
     f
      g
       h 
        i
         j 
"""

val ToTheRight= """
        
           g
         f
        e
      d
    c
  b
a
"""


}