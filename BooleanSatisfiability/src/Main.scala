
sealed trait Operator
case object Or extends Operator
case object And extends Operator
case object Xor extends Operator
case object Eq extends Operator

object ArrangeOperators { 
  import scala.math._
  
  def satisfy(operations: List[Operator], operands: List[Boolean], expected: Boolean): Option[Operation] = { 
    val allOps = permuteOperators(operations)
    val allAnds = permuteOperands(operands)
    
    permuteBoth(allOps, allAnds).find(_.evaluate == expected)
  }
  
  def findAllMixes(ops: List[Operator]): List[Operation] = { 
    val operands = permuteOperands(ops.size + 1)
    val operators = permuteOperators(ops)
    
    permuteBoth(operators, operands)
  } 
  
  def permuteBoth(operatorPossibilities: List[List[Operator]], operandPossibilities: List[List[Boolean]]): List[Operation] = { 
    
    operatorPossibilities.flatMap { operators => 
      operandPossibilities.flatMap { operands => 
        val simpleOperations = buildSimpleOperations(operators, operands)
        
        permuteOperations(simpleOperations)
      }
    }
  }
  
  def buildSimpleOperations(operations: List[Operator], operands: List[Boolean]): Seq[Operation] = {
    (0 to (operations.size - 1)).map { i => 
      Operation(operations(i), Right(operands(i)), Right(operands(i + 1)))
    }
  }
  
  /**
   * Takes a list of simple operations (true || false) and combines them.  Adjacent operations 
   * in the input list are assumed to have the same operators.  For example: 
   * 
   * [(true || false), (false && false), (false ^ true)] 
   */
  def permuteOperations(operations: Seq[Operation]): Seq[Operation] = {
    if (operations.size == 1) { return operations }
    
    // Iterating operation intersections
    (0 to (operations.size - 2)).map { i => 
        val a = operations(i)
        val b = operations(i + 1)
        
        val newOpLists = combineOperations(a, b).map { combinedOp => 
          (operations.take(i) :+ combinedOp) ++ operations.takeRight(operations.size - i - 2)
        }

        newOpLists.map(permuteOperations).flatten
    }.flatten
  }
  
  def combineOperations(a: Operation, b: Operation): List[Operation] = { 
     val aList = if (!b.hasLeftOperation) {
         List(new Operation(b.operator, Left(Operation(a.operator, a.left, a.right)), b.right))
     } else { List.empty }

     val bList = if (!a.hasRightOperation) {
         List(new Operation(a.operator, a.left, Left(Operation(b.operator, b.left, b.right))))
     } else { List.empty }
     
     aList ++ bList
  }
  
  def permuteOperators(operators: List[Operator]): List[List[Operator]] = { 
    val distinctOperators = operators.groupBy { o => o }.map { case (k, v) => (k, v.size ) }.toMap
    
    permute(distinctOperators)
  }
  
  private def permute[T](ops: Map[T, Int]): List[List[T]] = {
    if (ops.size == 1) { return List((1 to ops.toList.head._2).map { i => ops.toList.head._1 }.toList ) }
    
    val tree = ops.map { case (k, v) => 
      if (v > 1) {
        permute(ops + ((k, v - 1))).map { l => k :: l }
      } else { 
        permute(ops - k).map { l => k :: l }
      }
    }
    
    tree.foldLeft(List.empty[List[T]]) { case (c, a) => a ++ c }
  }
  
  def permuteOperands(ands: List[Boolean]): List[List[Boolean]] = {
    val boolsWithCount = ands.groupBy(identity).map { case (k, v) => (k, v.size) }.toMap
    
    permute(boolsWithCount)
  }
  
  def permuteOperands(cnt: Int): List[List[Boolean]] = {
    if (cnt == 1) { return List(List(true), List(false)) }
    
    val t = permuteOperands(cnt - 1).map { l => true :: l } 
    val f = permuteOperands(cnt - 1).map { l => false :: l }
    t ++ f
  }
}

case class Operation(
  operator: Operator,
  left: Either[Operation, Boolean],
  right: Either[Operation, Boolean]
) {
  
  def hasLeftOperation = left.left.toOption.isDefined
  def hasRightOperation = right.left.toOption.isDefined
  
  override def toString = {
    val lStr = left match { 
      case Left(op) => op.toString
      case Right(b) => b
    }
    
    val rStr = right match { 
      case Left(op) => op.toString
      case Right(b) => b
    }
    
    s"($lStr $operator $rStr)"
  }
  
  lazy val evaluate: Boolean = { 
    val lResult = left match { 
      case Left(op) => op.evaluate
      case Right(b) => b
    }
    
    val rResult = right match { 
      case Left(op) => op.evaluate
      case Right(b) => b
    }
    
    evaluteOp(operator, lResult, rResult)
  }
  
  private def evaluteOp(operator: Operator, left: Boolean, right: Boolean) = operator match { 
    case Or => left || right
    case And => left && right
    case Xor => left ^ right
    case Eq => left == right
  }
}

object Main extends App {
  import ArrangeOperators._
    
  def printPretty(tors: List[Operator], ands: List[Boolean], expected: Boolean) {
    val result = satisfy(tors, ands, expected)
    val andsStr = ands.mkString(", ")
    val torsStr = tors.mkString(", ")
    if (result.isDefined) { 
      println(s"Is it possible to arrange $andsStr with $torsStr to equal $expected?\n     Yes and the solution is: ${result.get.toString}" )
    } else { 
      println(s"Is it possible to arrange $andsStr with $torsStr to equal $expected?\n     No" )
    }
    println("")
    
  } 
  
  printPretty(List(And, Or), List(true, false, false), true)
  printPretty(List(And, Or), List(false, false, false), true)
  printPretty(List(And, And, And), List(false, true, true, true), true)
  printPretty(List(And, Xor), List(false, false, false), true)
  printPretty(List(And, Eq), List(false, false, false), true)
  printPretty(List(Xor, Eq), List(false, true, false), true)
  printPretty(List(Xor, Eq), List(false, true, true), true)
  printPretty(List(Xor, Eq), List(true, true, true), true)
  
}