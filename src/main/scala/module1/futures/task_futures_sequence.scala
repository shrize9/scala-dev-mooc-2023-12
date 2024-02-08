package module1.futures

import module1.futures.HomeworksUtils.TaskSyntax

import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений.
   * Не допускается использование методов объекта Await и мутабельных переменных var
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]])
                     (implicit ex: ExecutionContext): Future[(List[A], List[Throwable])] = {
    Future.foldLeft(
      futures.map {
        case ft=>
          ft.map(Left(_)).recover(Right(_))
      }
    )((List[A](), List[Throwable]())){
      case (accum, vl)=> vl match {
          case Left(_v) => (accum._1 ::: List(_v), accum._2)
          case Right(err) => (accum._1,  accum._2 ::: List(err))
      }
    }
  }

}
