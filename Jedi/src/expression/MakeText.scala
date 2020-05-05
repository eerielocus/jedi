package expression

import context._
import value._

case class MakeText(val body: Expression) extends SpecialForm {
  override def execute(env: Environment): Value = Text(body)
}