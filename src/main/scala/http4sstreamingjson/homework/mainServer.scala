package http4sstreamingjson.homework

import cats.effect.{IO, IOApp, Resource}
import http4sstreamingjson.homework
import org.http4s.{Request, Uri}
import org.http4s.client.Client
import org.http4s.ember.client.EmberClientBuilder

import scala.util.Try

object mainServer extends IOApp.Simple {

  override def run: IO[Unit] = {
    Restfull.server.use(_ => IO.never)
  }
}

object counterTestServer extends IOApp.Simple {
  val builder: Resource[IO, Client[IO]] = EmberClientBuilder.default[IO].build
  val requestCounter = Request[IO](uri = Uri.fromString("http://localhost:8080/counter").toOption.get)

  val resultCounter = builder.use(
    client => client.run(requestCounter).use(
      resp =>
        if (resp.status.isSuccess)
          resp.body.compile.to(Array).map(new String(_))
        else
          IO("Error")
    )
  )

  def run(): IO[Unit] = {
    import scala.concurrent.duration._
    for {
      _ <- homework.Restfull.server.use(_ =>
        resultCounter.flatMap(IO.println) *> IO.sleep(1.second)
          *> resultCounter.flatMap(IO.println) *>  IO.sleep(1.second)
          *> resultCounter.flatMap(IO.println) *>  IO.sleep(1.second)
          *> resultCounter.flatMap(IO.println) *>  IO.sleep(1.second)
      )
    } yield ()
  }
}

//Берем стрим для нашего slow запроса
object slowchunkTestStream extends IOApp.Simple {
  val total =100
  val chunk =10
  val time =5

  import scala.concurrent.duration._

  def run(): IO[Unit] = for{
    _ <- fs2.Stream((1 to total) : _*).chunkN(chunk).covary[IO].flatMap((chunk)=>fs2.Stream(chunk).covary[IO].delayBy(time.second)).evalMap((chunk)=>IO.println(chunk.toList.mkString(","))).compile.drain
  }yield ()
}

object slowchunkTestServer extends IOApp.Simple {
  val builder: Resource[IO, Client[IO]] = EmberClientBuilder.default[IO].build
  val requestSlow = Request[IO](uri = Uri.fromString("http://localhost:8080/slow/10/124/1").toOption.get)

  val resultSlow = builder.use(
    client => client.run(requestSlow).use(
      resp =>
        if (resp.status.isSuccess)
          resp.body.compile.to(Array).map(new String(_))
        else
          resp.body.compile.to(Array).map((t)=> "Exception: " + new String(t))
    )
  )

  def run(): IO[Unit] = {
    for {
      _ <- homework.Restfull.server.use(_ =>
        resultSlow.flatMap(IO.println) *> IO.never
      )
    } yield ()
  }
}
