package expression

import context._
import value._

case class Disjunction(list: List[Expression]) extends SpecialForm {
  override def execute(env: Environment): Value = {
    var res = false
    for(exp <- list if !res) {
      val bool = exp.execute(env)
      if(!bool.isInstanceOf[Boole]) throw new TypeException("Arguments to || must be Boole")
      res = bool.asInstanceOf[Boole].value || res
    }
    Boole(res)
  }
}