package tcof

/** Parent of clauses used in membership. */
abstract class Logical {
  protected type ValueType
  protected def value: ValueType

  def &&(other: Logical): Logical
  def ||(other: Logical): Logical
  def unary_!(): Logical
  def ->(other: Logical): Logical = !this || other
  def <->(other: Logical): Logical = this -> other && other -> this
}
