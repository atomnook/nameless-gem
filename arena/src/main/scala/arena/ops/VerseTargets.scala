package arena.ops

import protobuf.Target.{ALL, ENEMY_AT_RANDOM, FRIEND_AT_RANDOM, SELF, TARGET_NA, Unrecognized}
import protobuf.{CunitData, Target, Verse}

import scala.util.Random

case class VerseTargets(verse: Verse, friends: Seq[CunitData], enemies: Seq[CunitData], self: CunitData) {
  private[this] def pickOne[A](l: Seq[A]): Seq[A] = {
    if (l.isEmpty) {
      Nil
    } else {
      val at = Random.nextInt(l.size)
      l(at) :: Nil
    }
  }

  private[this] val friendsAlive = friends.filter(_.state.isAlive)

  private[this] val enemiesAlive = enemies.filter(_.state.isAlive)

  private[this] val map: Target => Seq[CunitData] = {
    case SELF => self :: Nil
    case ALL => friendsAlive ++ enemiesAlive
    case ENEMY_AT_RANDOM => pickOne(enemiesAlive)
    case FRIEND_AT_RANDOM => pickOne(friendsAlive.filterNot(_ == self))
    case TARGET_NA => Nil
    case Unrecognized(_) => Nil
  }

  val targets: Seq[CunitData] = map(verse.properties.target)
}
