package cz.cuni.mff.d3s.trust

/** Integer constraint type.
  *
  * Represents a value that can be used as an integer in the solver. This means that
  * constraints can be generated based on its value. Basic arithmetic operators are
  * overloaded, so it is possible to use `Integer` in a way similar to the native `Int`
  * type.
  *
  * Note that equality of `Integer`s must be tested with `===`, as it is impossible to
  * properly overload the built-in `==` operator.
  */
abstract class Integer {
  protected type ValueType
  protected def value: ValueType

  /** Return the concrete integer value, as determined in the current solution. */
  def asInt: Int

  def +(other: Integer): Integer
  def -(other: Integer): Integer
  def *(other: Integer): Integer
  def /(other: Integer): Integer
  def unary_-(): Integer
  def ===(num: Integer): Logical
  def !=(num: Integer): Logical
  def <(num: Integer): Logical
  def >(num: Integer): Logical
  def <=(num: Integer): Logical
  def >=(num: Integer): Logical
}
