import protobuf.Name.{NAMELESS, Unrecognized}
import protobuf.Target.TARGET_NA
import protobuf.Trigger.TRIGGER_NA

package object protobuf {
  private[this] val nameless = Properties()

  implicit class NameProperties(name: Name) {
    def properties: Properties = {
      name match {
        case NAMELESS => nameless

        case Unrecognized(_) => Properties()
      }
    }
  }

  implicit class PropertiesOps(props: Properties) {
    private[this] def option[A](f: Properties => A, none: A): Option[A] = {
      val a = f(props)
      if (a == none) None else Some(a)
    }

    def targetOption: Option[Target] = option(_.target, TARGET_NA)

    def triggerOption: Option[Trigger] = option(_.trigger, TRIGGER_NA)
  }

  implicit class VerseProperties(verse: Verse) {
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
}
