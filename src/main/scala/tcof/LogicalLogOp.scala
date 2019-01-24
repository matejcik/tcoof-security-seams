package tcof

import org.chocosolver.solver.constraints.nary.cnf.LogOp
import org.chocosolver.solver.constraints.nary.cnf.LogOp.Type
import org.chocosolver.solver.variables.BoolVar
import sun.reflect.generics.reflectiveObjects.NotImplementedException

/** And/Or tree of clauses. This is used to represent clauses about membership of a component. */
private[tcof] case class LogicalLogOp(value: LogOp) extends LogicalWithILogic {
  protected type ValueType = LogOp

  override def unary_!(): Logical = {
    throw new NotImplementedException
    // LogicalLogOp(LogOp.nand(value))
  }
}
