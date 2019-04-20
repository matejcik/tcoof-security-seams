package tcof

/** Result of an expression that can be directly instantiated (i.e. does not have to be represented as a variable in the solver. */
private[tcof] case class LogicalBoolean(value: Boolean) extends Logical {
  protected type ValueType = Boolean

  override def &&(other: Logical): Logical = if (!value) this else other

  override def ||(other: Logical): Logical = if (value) this else other

  override def unary_!(): Logical = LogicalBoolean(!value)
}
