
import scala.concurrent.Future

trait IO[F[_],E,A]{
  def init(f:F[Either[E,A]]): Unit = {
    println(f)
  }
}

def implIO[A]: IO[Future,Throwable, A] = new IO[Future,Throwable, A] {}

implIO[String].init(Future.successful(Right[Throwable,String]("ss")))