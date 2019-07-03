package tcof

abstract class Integer {
  protected type ValueType
  protected def value: ValueType

  def asInt: Int

  def +(other: Integer): Integer
  def -(other: Integer): Integer
  def *(other: Integer): Integer
  def /(other: Integer): Integer
  def ===(num: Integer): Logical
  def !=(num: Integer): Logical
  def <(num: Integer): Logical
  def >(num: Integer): Logical
  def <=(num: Integer): Logical
  def >=(num: Integer): Logical
}
