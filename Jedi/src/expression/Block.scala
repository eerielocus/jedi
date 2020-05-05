package expression

import context._
import value._

case class Block(var expressions: List[Expression]) extends SpecialForm {
  override def execute(env: Environment): Value = {
    var temp = new Environment(env)
    var res: List[Value] = expressions.map(exp => exp.execute(temp))
    res.last
  }
}