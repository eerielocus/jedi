package expression

import context._
import value._

case class Conjunction(val list: List[Expression]) extends SpecialForm {
  override def execute(env: Environment): Value = {
    var res = true
    for (exp <- list if res) {
      val bool = exp.execute(env)
      if (!bool.isInstanceOf[Boole]) throw new TypeException("Arguments to && must be Boole")
      res = bool.asInstanceOf[Boole].value && res
    }
    Boole(res)
  }
}