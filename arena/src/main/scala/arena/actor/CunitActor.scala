package arena.actor

import akka.actor.FSM
import arena.actor.CunitActor._
import arena.ops.{CunitAttributes, VerseTargets}
import protobuf.CunitData.CunitState
import protobuf.CunitData.CunitState.{ALIVE, DEAD}
import protobuf.{Cunit, CunitData, Verse}

class CunitActor(cunit: Cunit) extends FSM[CunitState, CunitData] {
  private[this] def init: CunitData = {
    val initial = CunitAttributes(cunit)
    CunitData().update(_.state := ALIVE, _.cunit := cunit, _.max := initial.attributes, _.current := initial.attributes)
  }

  private[this] def action(arg: Action, self: CunitData): Option[Verse] = {
    self.getCunit.actions.foldLeft(Option.empty[Verse]) { case (res, a) =>
      res.orElse {
        val vt = VerseTargets(verse = a, friends = arg.friends, enemies = arg.enemies, self = self)
        if (vt.targets.isEmpty) None else Some(vt.verse)
      }
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

  sealed trait ActionReplying
  case class ActionReply(verse: Option[Verse]) extends ActionReplying

  case object Rip extends ActionReplying
}
