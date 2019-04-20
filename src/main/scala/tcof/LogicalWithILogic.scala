package tcof

import org.chocosolver.solver.constraints.nary.cnf.{ILogical, LogOp}

/** Common functionality for LogicalLogOp and LogicalBoolVar. */
private[tcof] abstract class LogicalWithILogic extends Logical {
  protected type ValueType <: ILogical

  override def &&(other: Logical): Logical = other match {
    case LogicalBoolean(true)  => this
    case LogicalBoolean(false) => other
    case other: LogicalLogOp   => LogicalLogOp(LogOp.and(this.value, other.value))
    case other: LogicalBoolVar =>
      LogicalLogOp(LogOp.and(this.value, other.value))
  }

  override def ||(other: Logical): Logical = other match {
    case LogicalBoolean(false) => this
    case LogicalBoolean(true)  => other
    case other: LogicalLogOp   => LogicalLogOp(LogOp.or(this.value, other.value))
    case other: LogicalBoolVar =>
      LogicalLogOp(LogOp.or(this.value, other.value))
  }
}
