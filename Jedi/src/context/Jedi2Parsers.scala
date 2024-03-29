package context

import scala.util.parsing.combinator._
import expression._
import value._

class Jedi2Parsers extends Jedi1Parsers {
  
  // params parser
  // a parameter list is zero or more comma-separated identifiers bracketed by parentheses:
  // params ::= "(" ~ (identifier ~ ("," ~ identifier)*)? ~ ")"
  def params: Parser[List[Identifier]] = "(" ~> opt(identifier ~ rep("," ~> identifier)) <~ ")" ^^ {
    case Some(id ~ Nil) => List(id)
    case Some(id ~ more) => id::more
    case None => List()
  }
  
  // lambda parser
  // lambda ::= "lambda" ~ params ~ expression
  def lambda: Parser[Lambda] = "lambda" ~> params ~ expression ^^ {
    case par ~ exp => Lambda(par, exp)
  }
  
  // block parser
  // a block is one or more semi-colon separated expressions bracketed by curly braces:
  // block ::= "{" ~ expression ~ (";" ~ expression)* ~ "}"
  def block: Parser[Block] = "{" ~> expression ~ rep(";" ~> expression) <~ "}" ^^ {
    case exp ~ Nil => Block(List(exp))
    case exp ~ more => Block(exp::more)
  }
  
  // freeze parser
  // freeze ::= "freeze" ~ "(" ~ expression ~ ")" // makes a thunk
  def freeze: Parser[Expression] = "freeze" ~ "(" ~> expression <~ ")" ^^ {
    case exp => MakeThunk(exp)
  }
  
  // delay parser
  // delay ::= "delay" ~ "(" ~ expression ~ ")" // makes a text
  def delay: Parser[Expression] = "delay" ~ "(" ~> expression <~ ")" ^^ {
    case exp => MakeText(exp)
  }
  
  // override of term parser
  override def term: Parser[Expression]  = lambda | freeze | delay | funCall | block | literal | "("~>expression<~")"
}
