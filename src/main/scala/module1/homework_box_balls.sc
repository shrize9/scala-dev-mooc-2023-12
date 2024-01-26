
import scala.util.Random

type ColorBall =Byte
val BlackBall:ColorBall =0.toByte
val WhiteBall:ColorBall =1.toByte

class Box(val index:Int){
  private var balls =List[ColorBall](
                      BlackBall,BlackBall,BlackBall,
                      WhiteBall,WhiteBall,WhiteBall)

  def isEmpty = balls.isEmpty

  def getNextRandomBall(): Option[ColorBall] ={
    if(!isEmpty) {
      val _idx    = Random.nextInt(balls.size)
      val resBall = balls(_idx)
      balls = balls.patch(_idx, Nil, 1)//удаляем из списка выбранный шар
      //println(s"result of ${index} is ${resBall} - ${balls.size}")
      Some(resBall)
    }else
      None
  }
}

object BoxImpl{
  def isFirstBlackSecondWhite(box: Box):Boolean ={
    if(box.isEmpty)
      throw new Exception(s"box ${box.index} is empty")

    val res =for{
      r1 <-box.getNextRandomBall()
      r2 <-box.getNextRandomBall()
      if r1 == BlackBall && r2 == WhiteBall
    } yield {
      true
    }

    //println(res)
    //println("=============")
    res.getOrElse(false)
  }
}

import BoxImpl._

val countOfExperements =10000
val listOfResultExperements =
  for{i <-1 to countOfExperements} yield {
    isFirstBlackSecondWhite(new Box(i))
  }

println("Результат вытаскивания первым черный второй белым")
println(s"по формуле 3/5 =${3.toDouble/5}")
println(s"количество всего экспериментов ${listOfResultExperements.size}")
println(s"количество верных экспериментов ${listOfResultExperements.count(_ ==true)}")
println(s"результат вычисленный ${listOfResultExperements.count(_ ==true).toDouble / listOfResultExperements.size}")




