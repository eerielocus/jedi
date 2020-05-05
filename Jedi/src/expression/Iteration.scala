package expression

import context._
import value._

case class Iteration(val condition: Expression, val body: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    var res: Value = null
    try {
      while (condition.execute(env).asInstanceOf[Boole].value) res = body.execute(env)
      res
    } catch {
      case e: Exception => throw new TypeException("Must be a Boole.")
    }
  }
}