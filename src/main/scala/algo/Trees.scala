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
    case Empty => Node(value)
    case node@Node(nodeValue, _, right) if nodeValue < value  => node.copy(right =add(right)(value))
    case node@Node(nodeValue, left, _) if nodeValue >= value  => node.copy(left =add(left)(value))
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

  def maxOf(root:Tree, position: Position):Int={
    def _maxOf(_root:Tree, length:Int):Int = _root match {
      case Empty => length
      case Node(_, left, right)  => {
        val a =_maxOf(right, if((position ==isRight && right !=Empty)|| position == isCenter) length +1 else length)
        val b =_maxOf(left, if((position ==isLeft && left != Empty)|| position == isCenter) length +1 else length)
        if(position == isCenter)
          a + b
        else Math.max(a,b)
      }
      case _ =>
        throw new Exception("not defined")

    }
    _maxOf(root, 0)
  }

  def printSchema(root:Tree)= {
    val _countOfLevels =countOfLevels(root)
    val arrayTransformed =Array.fill[(Position,Option[Int])](_countOfLevels, Math.pow(2, _countOfLevels).toInt+1){(isCenter, None)}

    def transform(_root:Tree, position: Position, level:Int, parentColIndex:Int)(prepareRowsCols:Array[Array[(Position,Option[Int])]]): Array[Array[(Position,Option[Int])]] = {
      _root match {
        case Empty => prepareRowsCols
        case Node(nodeValue, left, right)  => (left, right) match {
          case (Empty, Empty) =>{
            println(s"$level value $nodeValue parentColIndex $parentColIndex")
            prepareRowsCols(level)(parentColIndex + position) =((position, Some(nodeValue)))
            prepareRowsCols
          }
          case (left:Tree, right:Tree) =>{
            println(s"$level value $nodeValue parentColIndex $parentColIndex -- shift (${maxOf(left, isRight)},${maxOf(right, isLeft)})")
            prepareRowsCols(level)(parentColIndex) =((position, Some(nodeValue)))
            transform(left, isLeft, level +1, parentColIndex +isLeft - maxOf(left, isRight))(prepareRowsCols)
            transform(right, isRight, level +1, parentColIndex + maxOf(right, isLeft))(prepareRowsCols)
          }
        }
      }
    }
    transform(root, isCenter, 0, (arrayTransformed(0).size /2))(arrayTransformed)

    arrayTransformed.foreach{
      case row=> {
        println(
          row.map{case (position, _)=> position match {
            case isLeft => "/"
            case isRight => "\\"
            case isCenter => " "
          }}.mkString("") +"\n"+
          row.map{case (_, value)=> {
            value.map(i=> i.toString).getOrElse("")
          }}.mkString(" ")
        )
      }
    }
  }
}

object Trees extends App{
  val values = { 10 :: 4 :: 16 :: 3 :: 6  :: 7 :: 17 :: 20 :: 15 :: 2 :: Nil}
  val root:Node   =values.tail.foldLeft(Node(values.head)){case (node:Tree, value)=> TreeOps.add(node)(value).asInstanceOf[Node]}

  println(s"levels in tree ${TreeOps.countOfLevels(root)}")
  println(s"length of root left shift in tree ${TreeOps.maxOf(root.left, TreeOps.isRight)}")
  println(s"length of root right shift in tree ${TreeOps.maxOf(root.right, TreeOps.isLeft)}")

  TreeOps.printSchema(root)
}
