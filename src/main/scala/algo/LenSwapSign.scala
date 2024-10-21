package algo

object LenSwapSign extends App{
  val samplesInput =
            Array(-1, 0, 1) :: Array(-2, -5, 10) :: Array(11, 23, -5, 0) :: Array(43, 0, 23, -2, -3, 4, -1) :: Nil


  def prepareArray(in:Array[String]) =
              in.map {case s=> s.toInt}.filter(_ !=0)

  def positiveNegativeCount(in:Array[Int])=
    in.tail.foldLeft((in.head,0)){
      case item => item match {
        case ((prevValue,accum), current) if prevValue.sign != current.sign =>
          (current, accum +1)
        case ((_,accum), current) =>
          (current, accum)
      }
    }._2

  samplesInput.foreach{
    case sampleInput=>{
      println(s"${sampleInput.mkString(" ")} = ${positiveNegativeCount(prepareArray(sampleInput.map(_.toString)))}")
    }
  }
}
