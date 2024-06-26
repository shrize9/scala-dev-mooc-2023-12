package AkkaEvents

import akka.NotUsed
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorSystem, Behavior, Props}
import akka.persistence.typed.PersistenceId
import akka.persistence.typed.scaladsl.{Effect, EventSourcedBehavior}

//book tickets
// 1. set up initial value
// 2. book
// 3. refund

object akka_example {
  //main commands
  sealed trait Command
  case class Book(number:Int) extends Command
  case class Refund(number:Int) extends Command

  //events
  sealed trait Event
  case class Booked(id: String, number: Int) extends Event
  case class Refunded(id: String, number: Int) extends Event

  //state
  case class State(value: Int) {
    def book(number: Int): State = copy(value = value - number)
    def refund(number: Int): State = copy(value = value + number)
  }

  object State {
    val empty = State(100)
  }

  object BookingSystem {
    def apply(): Behavior[Command] = Behaviors.setup{ctx =>
      val persId = "BookingSystem"
      EventSourcedBehavior[Command, Event, State] (
        PersistenceId.ofUniqueId(persId),
        State.empty,
        (state, command) => handleCommand(persId, state, command, ctx),
        (state, event) => handleEvent(state, event, ctx)
      )
    }
  }

  //Command
  def handleCommand(persistenceId: String,
                    state: State,
                    command: Command,
                    ctx: ActorContext[Command]): Effect[Event, State] = command match {
    case Book(number) =>
      ctx.log.info(s"event book $number, the total tickets count ${state.value}")
      val booked = Booked(persistenceId, number)
      Effect
        .persist(booked)
      .thenRun{ x =>
        ctx.log.info(s"the rest of the tickets is ${x.value}")
      }
    case Refund(number) =>
      ctx.log.info(s"event refund $number, the total tickets count ${state.value}")
      val refunded = Refunded(persistenceId, number)
      Effect
        .persist(refunded)
        .thenRun{x =>
          ctx.log.info(s"the rest tickets number is ${x.value}")
        }
  }

  //event
  def handleEvent(state: State, event: Event, ctx: ActorContext[Command]): State = event match {
    case Booked(_, number) =>
      ctx.log.info(s"receive event for tickets $number. The total count of the tickets is ${state.value}")
      state.book(number)
    case Refunded(_, number) =>
      ctx.log.info(s"receive event for tickets $number. the total tickets count is ${state.value}")
      state.refund(number)
  }

  def apply(): Behavior[NotUsed] = Behaviors.setup{ctx =>
    val bookingSystem = ctx.spawn(BookingSystem(), "BookingSyste", Props.empty)

    bookingSystem ! Book(5)
    bookingSystem ! Refund(2)
    bookingSystem ! Book(4)
    Behaviors.same
  }

  def main(args: Array[String]): Unit = {
    ActorSystem(akka_example(), "akka_typed")
  }
}
