package expression

import value._
import context._

case class Identifier(val name: String) extends Expression {
  override def toString = name
  override def execute(env: Environment) = env(this) match {
    case thunk: Thunk => thunk(env)
    case text: Text => text(env)
    case value: Value => value
  }
}