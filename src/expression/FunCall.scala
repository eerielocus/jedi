package expression

import context._
import value._

case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression {
  override def execute(env: Environment): Value = {
    if (env.contains(operator)) {
      val closure = env.apply(operator).asInstanceOf[Closure]
      closure.apply(operands.map(_.execute(env)))
    } else {
      alu.execute(operator, operands.map(_.execute(env)))
    }
  }
}