package value

import context._
import expression._

class Closure(val params: List[Identifier], val body: Expression, val defEnv: Environment) extends Value {
  def apply(args: List[Value], callEnv: Environment = null): Value = {
    if (args.length != params.length) throw new JediException("Number of arguments does not match.")
    val closure = {
      if (flags.staticScoping) new Environment(defEnv)
      else new Environment(callEnv)
    }
    closure.bulkPut(params, args)
    body.execute(closure)
  }
}