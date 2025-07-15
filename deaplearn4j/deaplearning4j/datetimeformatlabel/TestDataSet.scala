package deaplearning4j.datetimeformatlabel

import org.deeplearning4j.datasets.iterator.impl.IrisDataSetIterator

import java.util.{Calendar, Date}

object TestDataSet extends App{

  val dt=new DateGeneratorDataSet(new Date (2017-1900, 1, 1), new Date(2023 -1900,12,31), 32, 200)

  dt.sample()

  println(dt.tensor("вт, окт. 08 2019"))
  println(dt.tensor("17.10.2024"))

  println(dt.next())
  println("is ok")

  val iris =new IrisDataSetIterator()

  println(iris.next())
}
