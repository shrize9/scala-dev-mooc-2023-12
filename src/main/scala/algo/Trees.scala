package algo

import java.util
import scala.collection.mutable.ListBuffer

sealed trait Tree
case class Node(val value:Int, left:Tree=Empty, right:Tree=Empty) extends Tree
case object Empty extends Tree

object TreeOps{
  type Position =Int
  val isCenter:Position=0
  val isLeft:Position  = -1
  val isRight:Position = 1

  def add(root:Tree)(value:Int):Tree = root match {
    case Empty => println(s"create empty node with value: ${value}");Node(value)
    case node@Node(nodeValue, _, right) if nodeValue < value  => println(s"need add to right by ${nodeValue} add $value"); node.copy(right =add(right)(value))
    case node@Node(nodeValue, left, _) if nodeValue >= value  => println(s"need add to left by ${nodeValue} add $value"); node.copy(left =add(left)(value))
    case _ =>
      throw new Exception("not defined")
  }

  def countOfLevels(root:Tree):Int={
    def _countOfLevels(_root:Tree, levelAccum:Int):Int=_root match {
      case Empty => levelAccum
      case Node(_, left, right)  => {
        val a =_countOfLevels(right, levelAccum+1)
        val b =_countOfLevels(left, levelAccum+1)
        if(a>b) a else b
      }
      case _ =>
        throw new Exception("not defined")

    }
    _countOfLevels(root, 0)
  }

  def printSchema(root:Tree)= {
    val _countOfLevels =countOfLevels(root)
    val arrayTransformed =Array.fill[(Position,Option[Int])](_countOfLevels, Math.pow(2, _countOfLevels).toInt+1){(isCenter, None)}

    def transform(_root:Tree, position: Position, level:Int, parentColIndex:Int)(prepareRowsCols:Array[Array[(Position,Option[Int])]]): Array[Array[(Position,Option[Int])]] = {
      _root match {
        case Empty => prepareRowsCols
        case Node(nodeValue, right, left)  => {
          val currentCol =parentColIndex +position
          println(s"${nodeValue} - ${right} - $left - $level - $currentCol")

          arrayTransformed(level)(currentCol) =((position, Some(nodeValue)))

          transform(right, isRight,  level+1, currentCol)(prepareRowsCols)
          transform(left, isLeft,  level+1, currentCol)(prepareRowsCols)

          prepareRowsCols
        }
        case _ =>
          throw new Exception("not defined")
      }
    }
    transform(root, isCenter, 0, (arrayTransformed(0).size /2))(arrayTransformed)

    val symbols =Map(isCenter-> "c", isLeft ->"l", isRight ->"r")

    arrayTransformed.foreach{
      case row=> {
        println(
          row.map{case column=> {
            column._2.map(i=> symbols(column._1) + ":" +i.toString + " ").getOrElse(" ")
          }}.mkString(" ")
        )
      }
    }
  }
}

object Trees extends App{
  val values = { 10 :: 4 :: 16 :: 3 :: 6  :: 7 :: 17 :: 20 :: 15 :: Nil}
  val root   =values.tail.foldLeft(Node(values.head):Tree){case (node:Tree, value)=> TreeOps.add(node)(value)}

  println(s"levels in tree ${TreeOps.countOfLevels(root)}")

  TreeOps.printSchema(root)
}
