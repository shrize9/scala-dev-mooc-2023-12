package algo

import java.util
import scala.collection.mutable.ListBuffer

sealed trait Tree
case class Node(val value:Int, left:Tree=Empty, right:Tree=Empty) extends Tree
case object Empty extends Tree

object TreeOps{
  type Position =Int
  val isCenter:Position=0
  val isLeft:Position  =1
  val isRight:Position =2

  def add(root:Tree)(value:Int):Tree = root match {
    case Empty => Node(value)
    case node@Node(nodeValue, _, right) if nodeValue >= value  => node.copy(right =add(right)(value))
    case node@Node(nodeValue, left, _) if nodeValue < value  => node.copy(left =add(left)(value))
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
    val prepareRowsCols =Array.fill[ListBuffer[(Position,Int)]](_countOfLevels){new ListBuffer[(Position, Position)]()}

    def fillRowsCols(_root:Tree, position: Position, level:Int)(prepareRowsCols:Array[ListBuffer[(Position,Int)]]): Array[ListBuffer[(Position,Int)]] = {
      _root match {
        case Empty => prepareRowsCols
        case Node(nodeValue, Empty, Empty)  => {
          prepareRowsCols(level).addOne((position, nodeValue))
          prepareRowsCols
        }
        case Node(nodeValue, Empty, left)  => {
          prepareRowsCols(level).addOne((position, nodeValue))
          fillRowsCols(left, isLeft, level+1)(prepareRowsCols)
        }
        case Node(nodeValue, right, Empty)  => {
          prepareRowsCols(level).addOne((position, nodeValue))
          fillRowsCols(right, isRight,  level+1)(prepareRowsCols)
        }
        case Node(nodeValue, right, left)  => {
          prepareRowsCols(level).addOne((position, nodeValue))
          fillRowsCols(right, isRight,  level+1)(prepareRowsCols)
          fillRowsCols(left, isLeft,  level+1)(prepareRowsCols)
        }
        case _ =>
          throw new Exception("not defined")
      }
    }
    fillRowsCols(root, isCenter, 0)(prepareRowsCols)

    val offsetTabs =for{i<- 0 to _countOfLevels*2}yield{"\t"}
    prepareRowsCols.zipWithIndex.foreach{
      case (lb, idxRow) => {
        lb.toList.sortBy(_._1).zipWithIndex.foreach {
          case ((position: Position, value), idxCol) if position == isCenter =>
            print(offsetTabs.take({
              _countOfLevels / 2
            }).mkString("") + value)
          case ((position: Position, value), idxCol) if position == isLeft =>
            print(offsetTabs.take({
              _countOfLevels / 2 - idxRow
            }).mkString("") +"l(" +value +")")
          case ((position: Position, value), idxCol) if position == isRight =>
            print(offsetTabs.take({
              _countOfLevels / 2 + idxRow
            }).mkString("") +"r(" + value +")")
        }
        println("")
      }
    }
  }
}

object Trees extends App{
  val root =TreeOps.add(
              TreeOps.add(
                TreeOps.add(
                  TreeOps.add(Empty)(10)
                )(4)
              )(16)
            )(6)

  println(s"levels in tree ${TreeOps.countOfLevels(root)}")

  TreeOps.printSchema(root)
}
