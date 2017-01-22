import protobuf.Target.TARGET_NA
import protobuf.Trigger.TRIGGER_NA

package object protobuf {
  case class NameContext(map: Map[Name, Properties])

  implicit class NameProperties(name: Name)(implicit context: NameContext) {
    def properties: Properties = context.map.getOrElse(name, Properties())
  }

  implicit class PropertiesOps(props: Properties) {
    private[this] def option[A](f: Properties => A, none: A): Option[A] = {
      val a = f(props)
      if (a == none) None else Some(a)
    }

    def targetOption: Option[Target] = option(_.target, TARGET_NA)

    def triggerOption: Option[Trigger] = option(_.trigger, TRIGGER_NA)
  }

  implicit class VerseProperties(verse: Verse)(implicit context: NameContext) {
    private[this] def prop[A](f: Properties => A, default: A): A = {
      verse.names.reverse.map(_.properties).map(f).filterNot(_ == default).headOption.getOrElse(default)
    }

    def properties: Properties = {
      Properties().update(
        _.target := prop(_.target, TARGET_NA),
        _.trigger := prop(_.trigger, TRIGGER_NA),
        _.attributes := verse.names.flatMap(_.properties.attributes))
    }
  }

  implicit class AttributesOps(lhs: Attributes) {
    private[this] def ops(rhs: Attributes, op: (Long, Long) => Long): Attributes = {
      def tuple(f: Attributes => Long): (Long, Long) = (f(lhs), f(rhs))

      val o = op.tupled

      Attributes(
        hp = o(tuple(_.hp)),
        mp = o(tuple(_.mp)),
        str = o(tuple(_.str)),
        vit = o(tuple(_.vit)),
        dex = o(tuple(_.dex)),
        agi = o(tuple(_.agi)),
        int = o(tuple(_.int)),
        mnd = o(tuple(_.mnd)))
    }

    def add(rhs: Attributes): Attributes = ops(rhs, _ + _)
  }
}
