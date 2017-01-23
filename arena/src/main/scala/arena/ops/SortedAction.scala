package arena.ops

import arena.ops.SortedAction.ActionData
import protobuf.Trigger.{FIRST, LAST}
import protobuf.{CunitData, NameContext, Trigger, Verse}

case class SortedAction(actions: Seq[ActionData])(implicit context: NameContext) {
  private[this] def filterAndSort(trigger: Trigger): Seq[ActionData] = {
    actions.filter(_._2.exists(_.properties.trigger == trigger)).sortBy(_._1.getCurrent.agi)
  }

  val sorted: Seq[ActionData] = {
    val first = filterAndSort(FIRST)
    val last = filterAndSort(LAST).reverse
    val rest = actions.filterNot(a => first.contains(a) || last.contains(a))
    first ++ rest ++ last
  }
}

object SortedAction {
  type ActionData = (CunitData, Option[Verse])
}
