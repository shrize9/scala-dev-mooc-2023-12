import java.nio.file.Paths
import scala.io.Source
import scala.collection.parallel.CollectionConverters._

def formatPhone(phone: String)= {

  def formatChar(char:Char):Char = char match {
    case char if char.isDigit => '1'
    case char  => char
  }

  val result =(phone.head, phone.tail) match {
    case (head, tail) if head == '8' => '8' :: tail.map(formatChar).toList
    case (head, tail) if head == '7' => '7' :: tail.map(formatChar).toList
    case (head, tail) if head == '9' => '9' :: tail.map(formatChar).toList
    case (head, tail) if head.isDigit => '1' :: tail.map(formatChar).toList
    case (head, tail)  => head :: tail.map(formatChar).toList
  }
  result.mkString("")
}

val lstPhones =Source.fromFile("/Users/p_kuzmin/IdeaProjects/scala-dev-mooc-2023-12/phones.csv").getLines().toList.tail
lstPhones.par.map(formatPhone).groupBy((phone)=>phone).map{
  case (groupPhone, strings) =>(groupPhone, strings.length)
}.toList.sortBy((t)=>t._2).foreach{
  case (value, i) => println(s";${value};${value.length};$i")
}