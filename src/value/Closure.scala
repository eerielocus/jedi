package value

import context._
import expression._

class Closure(val params: List[Identifier], val body: Expression, val defEnv: Environment) extends Value {
  def apply(args: List[Value]): Value = {
    if (args.length != params.length) throw new JediException("Number of arguments does not match.")
    val closure = new Environment(defEnv)
    closure.bulkPut(params, args)
    body.execute(closure)
  }
}