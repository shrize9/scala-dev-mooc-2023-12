

val size ={Math.pow(2, 4) +1}.toInt
println(size)
for(i <- 0 until size){
    print("" + i +" ")
}
println()

println(s"center of ${{size/2}.toInt}")


import scala.concurrent.Future

trait IO[F[_],E,A]{
  def init(f:F[Either[E,A]]): Unit = {
    println(f)
  }
}

def implIO[A]: IO[Future,Throwable, A] = new IO[Future,Throwable, A] {}

implIO[String].init(Future.successful(Right[Throwable,String]("ss")))


trait Wrapper {
  def f():Unit

  def apply(): Unit = {
    val start = System.currentTimeMillis()
    f()
    val end = System.currentTimeMillis()
    println(s" time ${end -start}")
  }
}

object TestWrapper extends Wrapper{
  override def f()={
    Thread.sleep(1000)
  }
}

TestWrapper()

