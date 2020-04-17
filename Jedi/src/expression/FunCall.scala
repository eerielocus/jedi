package expression

import context._
import value._

case class FunCall(val id: Identifier, val exps: List[Expression]) extends Expression {
  override def execute(env: Environment): Value = {
    alu.execute(id, exps.map(exp => exp.execute(env)))
  }
}