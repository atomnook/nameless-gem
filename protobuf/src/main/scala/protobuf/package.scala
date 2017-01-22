import protobuf.Name.{NAMELESS, Unrecognized}

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

  implicit class VerseProperties(verse: Verse) {
    def properties: Properties = {
      Properties().update(
        _.target := ???,
        _.trigger := ???,
        _.attributes := verse.names.flatMap(_.properties.attributes))
    }
  }
}
