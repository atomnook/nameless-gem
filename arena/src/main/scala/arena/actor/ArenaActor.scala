package arena.actor

import akka.actor.{ActorRef, FSM, Props}
import arena.actor.ArenaActor.{ArenaDataAll, Settings, Start, Started}
import arena.actor.CunitActor.Action
import arena.ops.CunitAttributes
import protobuf.ArenaData.ArenaState
import protobuf.ArenaData.ArenaState.{ACTIVE, CLEANUP, GAME_CANCELED, GAME_OVER, GAME_START, SETUP}
import protobuf.CunitData.CunitState.ALIVE
import protobuf.{ArenaData, Cunit, CunitData, CunitId, NameContext, Verse}

import scala.concurrent.duration._

class ArenaActor(settings: Settings, arenaData: ArenaData, nameContext: NameContext)
  extends FSM[ArenaState, ArenaDataAll] {

  private[this] def cunitData(cunit: Cunit): CunitData = {
    val initial = CunitAttributes(cunit)
    CunitData().update(_.state := ALIVE, _.cunit := cunit, _.max := initial.attributes, _.current := initial.attributes)
  }

  private[this] def init: ArenaDataAll = {
    ArenaDataAll(
      left = arenaData.friends.map(cunitData),
      right = arenaData.enemies.map(cunitData),
      cunits = (arenaData.friends ++ arenaData.enemies).
        map(c => c.getId -> context.actorOf(CunitActor.props(nameContext), c.getId.id)).toMap,
      actions = Map.empty)
  }

  private[this] def find(id: CunitId, data: ArenaDataAll): CunitData = ???

  startWith(GAME_START, init)

  when(GAME_START, stateTimeout = settings.timeout) {
    case Event(StateTimeout, data) =>
      goto(GAME_CANCELED) using data

    case Event(Start, data) =>
      data.left.foreach(c => data.cunits(c.getId) ! Action(self = c, friends = data.left, enemies = data.right))
      data.right.foreach(c => data.cunits(c.getId) ! Action(self = c, friends = data.right, enemies = data.left))
      goto(SETUP) using data replying Started
  }

  when(SETUP, stateTimeout = settings.timeout) {
    case Event(StateTimeout, data) =>
      goto(GAME_CANCELED) using data
  }

  when(ACTIVE, stateTimeout = settings.timeout) {
    case Event(StateTimeout, data) =>
      goto(GAME_CANCELED) using data
  }

  when(CLEANUP, stateTimeout = settings.timeout) {
    case Event(StateTimeout, data) =>
      goto(GAME_CANCELED) using data
  }

  when(GAME_OVER, stateTimeout = settings.timeout) {
    case Event(StateTimeout, data) =>
      goto(GAME_CANCELED) using data
  }

  onTransition {
    case _ -> GAME_CANCELED => stop()
  }

  whenUnhandled {
    case Event(e, _) =>
      log.error("Received unhandled event: {}", e)
      goto(GAME_CANCELED)
  }
}

object ArenaActor {
  case class Settings(timeout: FiniteDuration)

  case class ArenaDataAll(left: Seq[CunitData], right: Seq[CunitData], cunits: Map[CunitId, ActorRef], actions: Map[CunitId, Option[Verse]])

  case object Start
  case object Started

  def props(settings: Settings, data: ArenaData, nameContext: NameContext): Props = Props(new ArenaActor(settings, data, nameContext))
}
