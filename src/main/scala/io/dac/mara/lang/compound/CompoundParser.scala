package io.dac.mara.lang.compound

import io.dac.mara.lang.parsers.BlockParser
import io.dac.mara.lang.root.LangParser
import org.parboiled2._

/**
  * Created by dcollins on 3/24/17.
  */
trait CompoundParser[E, Alg <: CompoundAlg[E]] extends BlockParser[E, Alg] with LangParser[E, Alg] {
  def Do: Rule1[E] = rule {
    "do" ~ Block ~> { (x: Seq[E]) =>
      alg.dox(x)
    }
  }

  def Empty: Rule1[E] = rule {
    MATCH ~> { () => alg.empty }
  }

}
