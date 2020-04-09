package expression

import context._
import value._

case class Conditional(condition: Expression, consequent: Expression, alternative: Expression = null) extends Expression {
  override def execute(env: Environment): Value = {
    val res: Value = condition.execute(env)
    if(!res.isInstanceOf[Boole]) Notification.UNSPECIFIED
    
    val bool = res.asInstanceOf[Boole].value
    if(bool) consequent.execute(env)
    else if(alternative != null) alternative.execute(env)
    else Notification.DONE
  }
}