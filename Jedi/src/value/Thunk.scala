package value

import context._
import expression._

case class Thunk(override val body: Expression, override val defEnv: Environment) extends Closure(Nil, body, defEnv) {
  var res: Value = null
  def apply(env: Environment) = {
    if (res == null) res = super.apply(List())
    res
  }
}