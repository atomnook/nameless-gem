package arena.ops

import protobuf.{Attributes, Cunit}

case class CunitAttributes(cunit: Cunit) {
  val attributes: Attributes = {
    cunit.latent.map(_.properties).foldLeft(Attributes()) { case (sum, prop) =>
        sum.add(prop.getLatent)
    }
  }
}
