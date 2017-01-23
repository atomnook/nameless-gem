package arena.ops

import protobuf.{CunitData, NameContext, Verse}

case class Action(self: CunitData, friends: Seq[CunitData], enemies: Seq[CunitData])(implicit context: NameContext) {
  val action: Option[Verse] = {
    if (self.state.isAlive) {
      self.getCunit.actions.foldLeft(Option.empty[Verse]) { case (res, a) =>
        res.orElse {
          val vt = VerseTargets(verse = a, friends = friends, enemies = enemies, self = self)
          if (vt.targets.isEmpty) None else Some(vt.verse)
        }
      }
    } else {
      None
    }
  }
}
