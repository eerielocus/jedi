package expression

import context._
import value._

case class Conditional(val condition: Expression, val consequent: Expression, val alternative: Expression = null) extends Expression {
  override def execute(env: Environment): Value = {
    val res = condition.execute(env)
    if (!res.isInstanceOf[Boole]) throw new TypeException("Condition must be a Boole.")
    
    if (res.asInstanceOf[Boole].value) consequent.execute(env)
    else if (alternative != null) alternative.execute(env)
    else Notification.UNSPECIFIED
  }
}