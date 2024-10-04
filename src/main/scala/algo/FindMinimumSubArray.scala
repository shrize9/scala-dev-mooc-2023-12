package algo

import scala.util.Random

object FindMinimumSubArray extends App {

  case class SubArray(star:Int, end:Int, sum:Int)

  val inputArray= for {i<-0 to 100} yield { -10 + Random.nextInt(30)}


  val result =for{x<- 0 to inputArray.length-1} yield{
    val sumX =inputArray(x)
    var sumY =sumX
    for{y <-x+1 to inputArray.length-1 } yield {
      sumY=sumY +inputArray(y)
      SubArray(x,y, sumY)
    }
  }

  println(
    result.flatten.maxBy(_.sum)
  )
  println(
    result.flatten.minBy(_.sum)
  )
}
