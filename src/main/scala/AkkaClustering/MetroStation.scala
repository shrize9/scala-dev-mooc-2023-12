package AkkaClustering

import akka.actor.Props
import akka.cluster.sharding.{ClusterSharding, ClusterShardingSettings, ShardRegion}
import com.typesafe.config.ConfigFactory
import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem}
import akka.cluster.sharding.external.ExternalShardAllocationStrategy.ShardRegion

import java.util.{Date, UUID}
import scala.util.Random


case class TroykaCard(id: String, isAllowed: Boolean)
case class EntryAttemp(troykaCard: TroykaCard, date: Date)
case class EntryRejected(reason: String)
case object EntryAccepted

class Turnstile(validator: ActorRef) extends Actor with ActorLogging {
  override def receive: Receive = {
    case o: TroykaCard =>
      log.info("validator")
      validator ! EntryAttemp(o, new Date)
    case EntryAccepted => log.info("Green")
    case EntryRejected(reason) => log.info(s"Red $reason")
  }

}

class TroykaCovidPassValidator extends Actor with ActorLogging {

  override def preStart(): Unit = {
    super.preStart()
    log.info("start checking")
  }

  override  def receive: Receive = {
    case EntryAttemp(card @ TroykaCard(_, isAllowed), _) =>
      log.info(s"validating $card")
      if (isAllowed) sender() ! EntryAccepted
      else sender() ! EntryRejected("not your day, sorry")

  }
}

object TurnstileSettings {
  val numberOfShards = 3
  val numberOfEntities = 30

  val extractEntityId: ShardRegion.ExtractEntityId = {
    case attemp @ EntryAttemp(TroykaCard(cardid, _), _) =>
      val entryId = cardid.hashCode % numberOfEntities
      println(s"!!! extract entry id for card # ${attemp.troykaCard.id} to entry ID ${entryId}")
      (entryId.toString, attemp)
  }

  val extractShardId: ShardRegion.ExtractShardId = {
    case EntryAttemp(TroykaCard(cardid, _), _) =>
      val shardId = cardid.hashCode % numberOfShards
      println(s"!!! extract shard id for card # ${cardid} to entry ID ${shardId}")
      shardId.toString
  }


}

class MetroStation(port: Int, amountOfTurnstile: Int) extends App {
  val config = ConfigFactory.parseString(
    s"""
       akka.remote.artery.canonical.port = $port
       |""".stripMargin).withFallback(ConfigFactory.load("clusterShardingExample.conf"))
  val system = ActorSystem("DemoCluster", config)

  val validationShardRegionRef: ActorRef =
    ClusterSharding(system).start(
      typeName = "TroykaCovidPassValidator",
      entityProps = Props[TroykaCovidPassValidator],
      settings = ClusterShardingSettings(system),
      extractEntityId = TurnstileSettings.extractEntityId,
      extractShardId = TurnstileSettings.extractShardId)

  val turnstiles: Seq[ActorRef] = (1 to amountOfTurnstile)
    .map{x =>
      println(s"Before starting actor of turnstiles # $x")
      system.actorOf(Props(new Turnstile(validationShardRegionRef)))
    }

  Thread.sleep(50000)
  for (_ <- 1 to 1000) {
    val randomTurnstileIndex = Random.nextInt(amountOfTurnstile)
    val randomTurnstile = turnstiles(randomTurnstileIndex)

    randomTurnstile ! TroykaCard(UUID.randomUUID().toString, Random.nextBoolean())
    Thread.sleep(200)
  }
}

object Station1 extends  MetroStation(2551, 10)
object Station2 extends  MetroStation(2561, 5)
object Station3 extends  MetroStation(2571, 15)