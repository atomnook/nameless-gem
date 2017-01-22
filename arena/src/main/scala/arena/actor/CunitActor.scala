package arena.actor

import akka.actor.FSM
import arena.actor.CunitActor._
import protobuf.CunitData.CunitState
import protobuf.CunitData.CunitState.{ALIVE, DEAD}
import protobuf.Target.{ALL, ENEMY_AT_RANDOM, FRIEND_AT_RANDOM, SELF, TARGET_NA, Unrecognized}
import protobuf.TargetOuterClass.Target
import protobuf.{Cunit, CunitData, Verse}

class CunitActor(cuint: Cunit) extends FSM[CunitState, CunitData] {
  private[this] def init: CunitData = CunitData().update(_.state := ALIVE, _.cunit := cuint)

  private[this] def meet(arg: Action, v: Verse): Option[Verse] = {
    v.names.map(_.properties.target).reverse.filterNot(_ == Target.TARGET_NA).headOption match {
      case None => None
      case Some(target) =>
        target match {
          case TARGET_NA => None
          case SELF => Some(v)
          case ALL => Some(v)
          case ENEMY_AT_RANDOM => Some(v)
          case FRIEND_AT_RANDOM => Some(v)
          case Unrecognized(value) => None
        }
    }
  }

  private[this] def action(arg: Action, data: CunitData): Option[Verse] = {
    data.getCunit.actions.foldLeft(Option.empty[Verse]) { case (res, a) =>
      res.orElse(meet(arg, a))
    }
  }

  startWith(ALIVE, init)

  when(ALIVE) {
    case Event(arg: Action, data) =>
      stay using data replying ActionReply(verse = action(arg, data))
  }

  when(DEAD) {
    case Event(_: Action, data) =>
      stay using data replying Rip
  }
}

object CunitActor {
  case class Action(friends: Seq[CunitData], enemies: Seq[CunitData])

  case class ActionReply(verse: Option[Verse])

  case object Rip
}
