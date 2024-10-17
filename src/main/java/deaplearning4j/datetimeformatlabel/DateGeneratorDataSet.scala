package deaplearning4j.datetimeformatlabel

import org.nd4j.linalg.dataset.DataSet
import org.nd4j.linalg.dataset.api.DataSetPreProcessor
import org.nd4j.linalg.dataset.api.iterator.DataSetIterator
import org.nd4j.linalg.factory.Nd4j

import java.text.SimpleDateFormat
import java.time.temporal.ChronoUnit
import java.util
import java.util.Date
import scala.util.Random


class DateGeneratorDataSet(val startDate:Date, val endDate:Date, val sizeBatch:Int, val examples:Int) extends DataSetIterator{
  private val formatters ="yyyy-MM-dd" :: "dd-MMM-yyyy" :: "dd/MM/yyyy" :: "E, MMM dd yyyy" :: "dd.MM.yy"  :: "dd.MM.yyyy" :: Nil

  private val word2index =new util.HashMap[Char,Int]()
  private val index2word =new util.HashMap[Int,Char]()
  private var n_words = 0

  def init() = {
    val diffDays:Int = ((endDate.getTime - startDate.getTime)/86400000).toInt
    val dates = for{i <-0 to examples} yield{
      startDate.toInstant.plus(Random.nextInt(diffDays), ChronoUnit.DAYS)
    }
    val ret =dates.map{case dt=>{
      val label =Random.nextInt(formatters.length-1)
      (new SimpleDateFormat(formatters(label)).format(Date.from(dt)), label)
    }}

    (ret, ret.maxBy(_._1.length)._1.length)
  }

  private var (pairs, maxInput)= init()
  initVocab()

  def initVocab(): Unit = {
    pairs.foreach{
      case (date, _) => date.foreach {
        case char if !word2index.containsKey(char)=>{
          n_words +=1
          word2index.put(char, n_words)
          index2word.put(n_words,char)
        }
        case _ =>
      }
    }
  }

  def sample(): Unit = {
    pairs.take(5).foreach{
      case (dt, label)=>
        println(s"$dt - $label")
    }
  }

  def tensor(date:String) = {
    val result =    Nd4j.zeros(1L, maxInput.toLong)
    date.map((char)=>word2index.get(char)).toList.zipWithIndex.foreach{
      case (idxValue, index)=> result.putScalar(0, index, idxValue)
    }
    result
  }

  override def inputColumns(): Int = maxInput

  override def totalOutcomes(): Int = formatters.size -1

  override def resetSupported(): Boolean = true

  override def asyncSupported(): Boolean = true

  override def reset(): Unit = {
    val result = init()
    pairs =result._1
    maxInput =result._2
    initVocab()
  }

  override def batch(): Int = sizeBatch

  override def setPreProcessor(preProcessor: DataSetPreProcessor): Unit = ???

  override def getPreProcessor: DataSetPreProcessor = ???

  override def getLabels: util.List[String] = ???

  private var cursor:Int =0
  override def hasNext: Boolean = {
    cursor < pairs.size
  }

  override def next(): DataSet = next(sizeBatch)

  override def next(batch: Int): DataSet = {
    val size   = if(pairs.size > cursor + batch) batch else pairs.size -1 -cursor
    val result = Nd4j.zeros(size, maxInput.toLong)
    val labels = Nd4j.zeros(size, {formatters.size -1}.toLong)

    for{i <-0 to size }{
      val (date, label) =pairs(cursor + i)
      result.putRow(i, tensor(date).getRow(0))
      labels.putScalar(label, 1)
    }

    cursor +=batch
    println(s"get next - $cursor - ${result.size(0)}")
    new DataSet(result, labels);
  }

}
