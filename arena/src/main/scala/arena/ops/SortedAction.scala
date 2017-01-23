package arena.ops

import protobuf.CunitDataOuterClass.CunitData
import protobuf.{NameContext, Verse}

case class SortedAction(actions: Seq[(CunitData, Option[Verse])])(implicit context: NameContext) {
  val sorted: Seq[(CunitData, Option[Verse])] = {
    actions.map(_._2.map(_.properties))
    ???
  }
}
