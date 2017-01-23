package arena.actor

import akka.actor.{ActorRef, FSM, Props}
import arena.actor.ArenaActor._
import arena.ops.{Action, CunitAttributes, SortedAction}
import protobuf.ArenaData.ArenaState
import protobuf.ArenaData.ArenaState.{ACTIVE, CLEANUP, GAME_CANCELED, GAME_OVER, GAME_START, SETUP}
import protobuf.CunitData.CunitState.ALIVE
import protobuf.{ArenaData, Cunit, CunitData, CunitId, NameContext, Verse}

import scala.concurrent.duration._

class ArenaActor(settings: Settings, arenaData: ArenaData)(implicit nameContext: NameContext)
  extends FSM[ArenaState, ArenaDataAll] {

  private[this] def cunitData(cunit: Cunit): CunitData = {
    val initial = CunitAttributes(cunit)
    CunitData().update(_.state := ALIVE, _.cunit := cunit, _.max := initial.attributes, _.current := initial.attributes)
  }

  private[this] def init: ArenaDataAll = {
    ArenaDataAll(
      left = arenaData.friends.map(cunitData),
      right = arenaData.enemies.map(cunitData),
      actions = Map.empty)
  }

  private[this] def get(id: CunitId, data: ArenaDataAll): CunitData = data.all.find(_.getId == id).get

  startWith(GAME_START, init)

  when(GAME_START, stateTimeout = settings.timeout) {
    case Event(StateTimeout, data) =>
      goto(GAME_CANCELED) using data

    case Event(Start, data) =>
      goto(SETUP) using data replying Started
  }

  when(SETUP, stateTimeout = settings.timeout) {
    case Event(StateTimeout, data) =>
      goto(GAME_CANCELED) using data

    case Event(Setup, data) =>
      val l = data.left.map(c => c.getId -> Action(friends = data.left, enemies = data.right, self = c).action)
      val r = data.right.map(c => c.getId -> Action(friends = data.right, enemies = data.left, self = c).action)
      goto(ACTIVE) using data.copy(actions = (l ++ r).toMap) replying SetupDone
  }

  when(ACTIVE, stateTimeout = settings.timeout) {
    case Event(StateTimeout, data) =>
      goto(GAME_CANCELED) using data

    case Event(Act, data) =>
      val actions = data.actions.toSeq.map { case (id, verse) =>
        (get(id, data), verse)
      }
      val next = SortedAction(actions).sorted.headOption match {
        case Some((cur, _)) =>
          // todo: use verse
          data.actions.filterNot(_._1 == cur.getId)

        case None =>
          log.warning("maybe unreachable code")
          Map.empty[CunitId, Option[Verse]]
      }
      if (next.isEmpty) {
        goto(CLEANUP) using data.copy(actions = next) replying ActDone
      } else {
        stay using data.copy(actions = next) replying ActContinue
      }
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

  case class ArenaDataAll(left: Seq[CunitData], right: Seq[CunitData], actions: Map[CunitId, Option[Verse]]) {
    def all: Seq[CunitData] = left ++ right
  }

  case object Start
  case object Started

  case object Setup
  case object SetupDone

  case object Act
  case object ActContinue
  case object ActDone

  def props(settings: Settings, data: ArenaData, nameContext: NameContext): Props = Props(new ArenaActor(settings, data)(nameContext))
}
