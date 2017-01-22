package arena.actor

import akka.actor.{ActorRef, FSM, Props}
import arena.actor.ArenaActor.ArenaDataAll
import protobuf.ArenaData.ArenaState
import protobuf.ArenaData.ArenaState.{ACTIVE, CLEANUP, GAME_OVER, GAME_START, SETUP}
import protobuf.{ArenaData, Cunit, NameContext}

class ArenaActor(data: ArenaData, nameContext: NameContext) extends FSM[ArenaState, ArenaDataAll] {

  private[this] def init: ArenaDataAll = {
    ArenaDataAll(
      data = data,
      cunits = (data.friends ++ data.enemies).
        map(c => c -> context.actorOf(CunitActor.props(c, nameContext), c.id)).toMap)
  }

  startWith(GAME_START, init)

  when(GAME_START)(???)

  when(SETUP)(???)

  when(ACTIVE)(???)

  when(CLEANUP)(???)

  when(GAME_OVER)(???)
}

object ArenaActor {
  case class ArenaDataAll(data: ArenaData, cunits: Map[Cunit, ActorRef])

  def props(data: ArenaData, nameContext: NameContext): Props = Props(new ArenaActor(data, nameContext))
}
