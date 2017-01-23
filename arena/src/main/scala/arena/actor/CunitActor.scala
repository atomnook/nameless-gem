package arena.actor

import akka.actor.{FSM, Props}
import arena.actor.CunitActor._
import arena.ops.VerseTargets
import protobuf.CunitData.CunitState
import protobuf.CunitData.CunitState.{ALIVE, DEAD}
import protobuf.{CunitData, CunitId, NameContext, Verse}

class CunitActor()(implicit nameContext: NameContext) extends FSM[CunitState, Unit] {
  private[this] def action(arg: Action): Option[Verse] = {
    arg.self.getCunit.actions.foldLeft(Option.empty[Verse]) { case (res, a) =>
      res.orElse {
        val vt = VerseTargets(verse = a, friends = arg.friends, enemies = arg.enemies, self = arg.self)
        if (vt.targets.isEmpty) None else Some(vt.verse)
      }
    }
  }

  startWith(ALIVE, ())

  when(ALIVE) {
    case Event(arg: Action, data) =>
      stay using data replying ActionReply(self = arg.self.getId, verse = action(arg))
  }

  when(DEAD) {
    case Event(arg: Action, data) =>
      stay using data replying Rip(self = arg.self.getId)
  }
}

object CunitActor {
  case class Action(self: CunitData, friends: Seq[CunitData], enemies: Seq[CunitData])

  sealed trait ActionReplying
  case class ActionReply(self: CunitId, verse: Option[Verse]) extends ActionReplying

  case class Rip(self: CunitId) extends ActionReplying

  def props(context: NameContext): Props = Props(new CunitActor()(context))
}
