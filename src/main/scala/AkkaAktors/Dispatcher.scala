package AkkaAktors

import akka.NotUsed
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import AkkaAktors.Dispatcher.JsonParser.{Parse, ParseResponse}
import AkkaAktors.Dispatcher.LogWorker.{Log, LogRequest, LogResponse}
import AkkaAktors.Dispatcher.TaskDispatcher.{LogWork, ParseUrl}

import java.util.UUID

object Dispatcher extends App {
  //main actor
  object TaskDispatcher {
    sealed trait CommandDispatcher

    case class ParseUrl(url: String) extends CommandDispatcher
    case class LogWork(s: String) extends CommandDispatcher

    case class LogResponseWrapper(msg: LogResponse) extends CommandDispatcher
    case class ParseResponseWrapper(msg: ParseResponse) extends CommandDispatcher

    def apply(): Behavior[CommandDispatcher] = Behaviors.setup{ctx =>
      val logAdapter: ActorRef[LogResponse] = ctx.messageAdapter[LogResponse](rs => LogResponseWrapper(rs))
      val parseAdapter: ActorRef[ParseResponse] = ctx.messageAdapter[ParseResponse](rs => ParseResponseWrapper(rs))

      Behaviors.receiveMessage{
        case LogWork(work) =>
          val logWorker: ActorRef[LogRequest] = ctx.spawn(LogWorker(), s"LogWorkerNo${UUID.randomUUID()}")
          ctx.log.info(s"Dispatcher received log $work")
          logWorker ! LogWorker.Log(work, logAdapter)
          Behaviors.same
        case ParseUrl(url) =>
          val urlParser = ctx.spawn(JsonParser(), s"JsonParserNo${UUID.randomUUID()}")
          ctx.log.info(s"Dispatcher received url $url")
          urlParser ! Parse(url, parseAdapter)
          Behaviors.same
        case LogResponseWrapper(m) =>
          ctx.log.info("Log done")
          Behaviors.same
        case ParseResponseWrapper(m) =>
          ctx.log.info("Parse done")
          Behaviors.same
      }
    }

  }

  //related actors
  object LogWorker {
    sealed trait  LogRequest
    case class Log(s: String, replyTo: ActorRef[LogResponse]) extends LogRequest

    sealed trait LogResponse
    case class LogDone() extends  LogResponse

    def apply(): Behavior[LogRequest] = Behaviors.setup{ctx =>
      Behaviors.receiveMessage{
        case Log(s, replyTo) =>
          ctx.log.info("log work in progress")
          replyTo ! LogDone()
          Behaviors.stopped
      }
    }

  }

  object JsonParser {
    sealed trait  ParseCommand
    case class Parse(json: String, replyTo: ActorRef[ParseResponse]) extends ParseCommand

    sealed trait ParseResponse
    case class ParseDone() extends  ParseResponse

    def apply(): Behavior[ParseCommand] = Behaviors.setup{ctx =>
      Behaviors.receiveMessage{
        case Parse(json, replyTo) =>
          ctx.log.info(s"parsing $json done")
          replyTo ! ParseDone()
          Behaviors.stopped
      }
    }


  }

  def apply(): Behavior[NotUsed] =
    Behaviors.setup{ctx =>
      val dispatcherActorRef = ctx.spawn(TaskDispatcher(), "disp_root")
      dispatcherActorRef ! LogWork("bla bla bla")
      dispatcherActorRef ! ParseUrl("url url url")
      Behaviors.same
    }

  implicit  val system = ActorSystem(Dispatcher(), "disp_actor_system")

}
