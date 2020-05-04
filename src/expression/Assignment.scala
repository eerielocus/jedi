package expression

import context._
import value._

case class Assignment(val name: Identifier, val expression: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    if (env(name).isInstanceOf[Variable]) {
      env(name).asInstanceOf[Variable].value = expression.execute(env)
      Notification.DONE
    }
    else throw new TypeException("Must be a variable.")
  }
}