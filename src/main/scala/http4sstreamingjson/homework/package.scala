package http4sstreamingjson

import cats.effect.{IO, IOApp, Ref, Resource}
import com.comcast.ip4s.{Host, Port}
import org.http4s.{Http, HttpRoutes}
import org.http4s.dsl.io._
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Router
import io.circe.{Decoder, Encoder}
import io.circe.derivation.{deriveDecoder, deriveEncoder}
import cats.effect.unsafe.implicits.global

import fs2.{Chunk, Pure, Stream}

package object homework {
  case class Counter(counter:Long)
  implicit val encoderCounter: Encoder[Counter] = deriveEncoder

  object Restfull{
   def serviceCounter(counter:Ref[IO,Counter]): HttpRoutes[IO] =
     HttpRoutes.of {
       case GET -> Root / "counter"=> {
         val _counter =counter.getAndUpdate((t)=>{
           new Counter(t.counter +1)
         })
         Ok(_counter.map(encoderCounter(_).noSpaces))
       }
     }

   val serviceSlowChunk:HttpRoutes[IO] = HttpRoutes.of {
     case GET -> Root / "slow" / chunk / total / time => {
       import scala.concurrent.duration._

       val stream =Stream((1 to total.toInt) : _*).chunkN(chunk.toInt).covary[IO]
         .evalMap((chunk)=> {
            IO.sleep(2.second) *> IO.delay(chunk.toList.map(_.toByte).mkString(""))
         })
       Ok(stream)
     }
   }

   def routes(counter:Ref[IO,Counter]) = Router("/" -> serviceCounter(counter), "/" ->serviceSlowChunk)
   def httpApp(counter:Ref[IO,Counter]): Http[IO,IO] = routes(counter).orNotFound

   val server = for {
     counter <-Resource.eval(Ref.of[IO, Counter](new Counter(0)))
     s <- EmberServerBuilder
       .default[IO]
       .withPort(Port.fromInt(8080).get)
       .withHost(Host.fromString("localhost").get)
       .withHttpApp(httpApp(counter)).build
   } yield  s
 }
}
