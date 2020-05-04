package value

import expression._

case class Boole(val value: Boolean) extends Literal with Equals {
  def &&(other: Boole) = Boole(this.value && other.value)
  def ||(other: Boole) = Boole(this.value || other.value)
  def unary_! = Boole(!this.value)
  
  override def toString = if(value) "true" else "false"
  override def canEqual(other: Any) = other.isInstanceOf[Boole]
  override def equals(other: Any): Boolean =
    other match {
      case other: Boole => this.canEqual(other) && (other.value == this.value)
      case _ => false
  }
  
  override def hashCode = this.toString.##
}