


def drawLine()={
  println((0 to 20).map(_=>"-").mkString(""))
}

object option{
  sealed trait Option[+T]{

    def isEmpty: Boolean = this match {
      case None => true
      case Some(v) => false
    }

    def get: T = this match {
      case Some(v) => v
      case None => throw new Exception("get on empty Option")
    }

    def map[B](f: T => B): Option[B] = flatMap(v => Option(f(v)))

    def flatMap[B](f: T => Option[B]): Option[B] = this match {
      case Some(v) => f(v)
      case None => None
    }


    /**
     *
     * Реализовать метод printIfAny, который будет печатать значение, если оно есть
     */
    def printIfAny(): Unit =this match {
      case Some(v) => println(v)
      case None => ()
    }


    /**
     *
     * Реализовать метод zip, который будет создавать Option от пары значений из 2-х Option
     */
    def zip[A](value:Option[A]):Option[(T,A)]= this match {
      case Some(v) if value !=None => Option((v, {value:Option[A]}.get))
      case _=> None
    }

    /**
     *
     * Реализовать метод filter, который будет возвращать не пустой Option
     * в случае если исходный не пуст и предикат от значения = true
     */
    def filter(f:T=>Boolean):Option[T]= this match {
      case Some(v) if f(v) => Option(v)
      case _=> None
    }
  }

  case class Some[T](v: T) extends Option[T]
  case object None extends Option[Nothing]

  object Option{
    def apply[T](v: T): Option[T] = Some(v)
  }
}

drawLine()
println("ПРОВЕРКА МЕТОДОВ OPTION")
drawLine()
val opt =Some(2).zip(Some("string"))
println(opt)
drawLine()
val opt2 =Some(2).filter(i=>i %2 ==0)
println(opt2)
val opt3 =Some(3).filter(i=>i %2 ==0)
println(opt3)


object list {
  /**
   *
   * Реализовать односвязанный иммутабельный список List
   * Список имеет два случая:
   * Nil - пустой список
   * Cons - непустой, содердит первый элемент (голову) и хвост (оставшийся список)
   */

  sealed trait List[+T]{
    /**
     * Метод cons, добавляет элемент в голову списка, для этого метода можно воспользоваться названием `::`
     *
     */
    def :: [TT >: T](elem: TT): List[TT] = Cons(elem, this)
    def cons [TT >: T](elem: TT): List[TT] = Cons(elem, this)

    /**
     * Метод помощник обходит список и вызывает процедуру для каждого элемента списка
     * @param a
     * @param itemCallback
     * @tparam T
     */
    private def loop[T](a:List[T], itemCallback:List[T]=>Unit):Unit = a match {
      case Nil => {itemCallback(Nil)}
      case cons@Cons(_, _) => {
        itemCallback(cons)
        loop(cons.tail, itemCallback)
      }
    }

    /**
     * Метод помощник обходит список и создает новый список, значения которые результат переданной функции
     * @param a
     * @param itemCallback
     * @tparam T
     */
    private def loopWithResult[T, B](a:List[T], itemCallback:(T)=>B):List[B] = a match {
      case Nil => {Nil}
      case cons@Cons(head, Nil) => {
        Cons(itemCallback(head), Nil)
      }
      case cons@Cons(head, tail) => {
        Cons(itemCallback(head), loopWithResult(tail, itemCallback))
      }
    }

    /**
     * Метод помощник обходит список и создает новый список, из значений которые соответствуют условию itemCallback
     * @param a
     * @param itemCallback
     * @tparam T
     */
    private def loopFilterByCond[T](a:List[T], itemCallback:(T)=>Boolean):List[T] = a match {
      case Nil => {Nil}
      case Cons(head, Nil) if(itemCallback(head))=> {
        Cons(head, Nil)
      }
      case Cons(head, Nil) if(!itemCallback(head))=> {
        Nil
      }
      case Cons(head, tail) if(itemCallback(head))=> {
        Cons(head, loopFilterByCond(tail, itemCallback))
      }
      case Cons(head, tail) if(!itemCallback(head)) =>
        loopFilterByCond(tail, itemCallback)
    }

    /**
     * соединение двух списков
     * @param elements
     * @tparam TT
     * @return
     */
    def ::: [TT >: T](elements: List[TT]): List[TT] = {
      this match {
        case Nil =>
          elements
        case Cons(head, Nil)=>
          Cons(head, elements)
        case Cons(head, tail) =>{
          def loopWithResult(a:List[TT]):List[TT] = a match {
            case Nil => {Nil}
            case cons@Cons(head, Nil) => {
              Cons(head, elements)
            }
            case cons@Cons(head, tail) => {
              Cons(head, loopWithResult(tail))
            }
          }

          Cons(head, loopWithResult(tail))
        }
      }
    }

    /**
     * Метод mkString возвращает строковое представление списка, с учетом переданного разделителя
     *
     */
    def mkString[TT >: T](separator:String):String={
      val sb=new StringBuilder()
      loop(this, (item:List[T])=> item match {
        case Nil =>{}
        case Cons(head, tail)=> {
          sb.append(head.toString)
          sb.append(separator)
        }
      })

      if(!sb.isEmpty){
        sb.delete(sb.length() -separator.length, sb.length())
      }
      sb.toString()
    }

    /**
     *
     * Реализовать метод reverse который позволит заменить порядок элементов в списке на противоположный
     */
    def reverse [TT >: T](): List[T] ={
      def _reverse(a:List[T]): List[T] ={
        a match {
          case Nil => {Nil}
          case Cons(head, tail) => {
            List(head) ::: _reverse(tail)
          }
        }
      }

      _reverse(this)
    }

    /**
     *
     * Реализовать метод map для списка который будет применять некую ф-цию к элементам данного списка
     */

    def map [TT >: T, A](f:T=>A): List[A] ={
      loopWithResult(this, f)
    }

    /**
     *
     * Реализовать метод filter для списка который будет фильтровать список по некому условию
     */
    def filter [TT >: T](f:T=>Boolean): List[T] ={
      loopFilterByCond(this, f)
    }

    def forEach [TT >: T](f:T=>Unit): Unit ={
      loop(this, (item:List[T]) => item match {
        case Nil =>{}
        case Cons(head, tail)=>
          f(head)
      })
    }
  }

  case class Cons[A](head: A, tail: List[A]) extends List[A]
  case object Nil extends List[Nothing]

  /**
   * Конструктор, позволяющий создать список из N - го числа аргументов
   * Для этого можно воспользоваться *
   *
   * Например вот этот метод принимает некую последовательность аргументов с типом Int и выводит их на печать
   * def printArgs(args: Int*) = args.foreach(println(_))
   */

  object List{
    def apply[A](v: A*): List[A] =
      if(v.isEmpty) Nil
      else Cons(v.head, apply(v.tail:_*))

  }

}

val t1 =Cons(1, Nil) // List(1)
val t2 =Cons(1, Cons(2, Nil)) // List(1, 2, 3)
val t3 =Cons(1, Cons(2, Cons(3, Nil))) // List()
val list =List(3, 4, 5) // List()

drawLine()
println("ПРОВЕРКА ВСЕХ МЕТОДОВ СПИСКА")
drawLine()
println("append element to list")
println({1 :: list })
println({list cons 1})

drawLine()
println("mkString result variable \"list\"")
println(list.mkString("||"))

drawLine()
println("concat two list result variables \"list\" and \"t2\"")
println( list ::: t2)
println( (list ::: t2).mkString("||"))

drawLine()
println("reverse result for variable \"list\"")
println(list.reverse())
println(list.reverse().mkString(" "))

drawLine()
println("map result variable \"list\" and new list is pow")
println(list.map((i:Int)=>{i*i}).mkString(","))

drawLine()
println("filter is odd result (List(2,6) ::: list)")
println((List()).filter((i:Int)=> i % 2 ==0 ).mkString(" "))
println((List(2,6) ::: list).filter((i:Int)=> i % 2 ==0 ).mkString(" "))

drawLine()
println("КОНЕЦ ПРОВЕРКИ ВСЕХ МЕТОДОВ СПИСКА")
drawLine()

/**
 *
 * Написать функцию incList котрая будет принимать список Int и возвращать список,
 * где каждый элемент будет увеличен на 1
 */

def incList(items:List[Int]): List[Int] ={
  items.map((item)=> item +1)
}
println(incList(list).mkString(" "))

/**
 *
 * Написать функцию shoutString котрая будет принимать список String и возвращать список,
 * где к каждому элементу будет добавлен префикс в виде '!'
 */
def shoutString(items:List[String]): List[String] ={
  items.map((item)=> item +"!")
}
println(shoutString(List("hello","world")).mkString(" "))








