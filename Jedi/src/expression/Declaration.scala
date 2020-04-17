package expression

import value._
import context._

case class Declaration(val id: Identifier, val exp: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = {
    env(id) = exp.execute(env)
    Notification.OK
  }
}