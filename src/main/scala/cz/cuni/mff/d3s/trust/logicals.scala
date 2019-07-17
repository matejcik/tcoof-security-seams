package cz.cuni.mff.d3s.trust

import org.chocosolver.solver.constraints.nary.cnf.{ILogical, LogOp}
import org.chocosolver.solver.variables.BoolVar
import sun.reflect.generics.reflectiveObjects.NotImplementedException

/** Constraint type.
  *
  * Represents a truth value that can be used by the solver. This means that
  * instances of this class can be used as constraints directly. Boolean operators are
  * overloaded, so it is possible to use `Logical` in a way similar to the native
  * `Boolean` type.
  */
abstract class Logical {
  protected type ValueType
  protected def value: ValueType

  /** Logical conjunction. */
  def &&(other: Logical): Logical

  /** Logical disjunction. */
  def ||(other: Logical): Logical

  /** Logical negation. */
  def unary_!(): Logical

  /** Implication: if `this` is true, then `other` must also be true. */
  def ->(other: Logical): Logical = !this || other

  /** Equivalence: truth value of `this` is identical to `other`. */
  def <->(other: Logical): Logical = this -> other && other -> this
}

/** Solver-bound constraint type.
  *
  * Common functionality for LogicalLogOp and LogicalBoolVar, based on Choco solver's
  * [[ILogical]] interface.
  * */
private[trust] abstract class LogicalWithILogic extends Logical {
  protected type ValueType <: ILogical

  override def &&(other: Logical): Logical = other match {
    case LogicalBoolean(true)     => this
    case LogicalBoolean(false)    => other
    case other: LogicalWithILogic => LogicalLogOp(LogOp.and(this.value, other.value))
  }

  override def ||(other: Logical): Logical = other match {
    case LogicalBoolean(false)    => this
    case LogicalBoolean(true)     => other
    case other: LogicalWithILogic => LogicalLogOp(LogOp.or(this.value, other.value))
  }
}

/** Statically known constraint type.
  *
  * Wrapper around the native `Boolean` type, interoperable with solver-bound types.
  */
private[trust] case class LogicalBoolean(value: Boolean) extends Logical {
  protected type ValueType = Boolean

  override def &&(other: Logical): Logical = if (!value) this else other
  override def ||(other: Logical): Logical = if (value) this else other
  override def unary_!(): Logical = LogicalBoolean(!value)
}

/** Native solver constraint.
  *
  * Wrapper around Choco solver [[BoolVar]] type. Can represent reified constraints,
  * such as membership conditions or results of arithmetic comparisons.
  */
private[trust] case class LogicalBoolVar(value: BoolVar) extends LogicalWithILogic {
  protected type ValueType = BoolVar

  override def unary_!(): Logical =
    LogicalBoolVar(value.not.asInstanceOf[BoolVar])
}

/** Tree of boolean operations on constraints
  *
  * Wrapper around Choco solver [[LogOp]] type. Represents results of boolean operations
  * with [[BoolVar]]s.
  */
private[trust] case class LogicalLogOp(value: LogOp) extends LogicalWithILogic {
  protected type ValueType = LogOp

  override def unary_!(): Logical = {

    throw new NotImplementedException
    // LogicalLogOp(LogOp.nand(value))
  }
}
