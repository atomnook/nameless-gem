package arena.ops

import protobuf.{Attributes, Cunit, NameContext}

case class CunitAttributes(cunit: Cunit)(implicit context: NameContext) {
  val attributes: Attributes = {
    cunit.latent.map(_.properties).foldLeft(Attributes()) { case (sum, prop) =>
        sum.add(prop.getLatent)
    }
  }
}
