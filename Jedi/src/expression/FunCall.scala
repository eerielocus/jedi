package expression

import context._
import value._

case class FunCall(val operator: Identifier, val operands: List[Expression]) extends Expression {
  override def execute(env: Environment): Value = {
    val args: List[Value] = flags.paramPassing match {
      case flags.byName => operands.map(new Thunk(_, env))
      case flags.byText => operands.map(new Text(_))
      case flags.byValue => operands.map(_.execute(env))
    }
    if (env.contains(operator)) {
      operator.execute(env) match {
        case f: Thunk => f(env)
        case f: Closure => f.apply(args, env)
        case _ => throw new TypeException("Only functions can be called.")
      }
    } else {
      if (flags.paramPassing != flags.byValue)
        alu.execute(operator, operands.map(_.execute(env)))
      else
        alu.execute(operator, args)
    }
  }
}