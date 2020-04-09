package expression

import context._
import value._

case class FunCall(op: Identifier, operands: List[Expression]) extends Expression {
  override def execute(env: Environment): Value = {
    alu.execute(op, operands.map(operand => operand.execute(env)))
  }
}