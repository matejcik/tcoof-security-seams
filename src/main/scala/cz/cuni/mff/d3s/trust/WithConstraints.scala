package cz.cuni.mff.d3s.trust

import scala.collection.mutable

/** Ensemble part for handling constraints.
  *
  * Provides DSL functions that register constraints.
  */
trait WithConstraints extends Initializable {

  /** Internal list of constraint generator functions. */
  private[trust] val _constraintsClauseFuns = mutable.ListBuffer.empty[() => Logical]

  /** Register a constraint.
    *
    * The `clause` is a `Logical` expression which is only evaluated when the ensemble hierarchy
    * is initialized. This way, it is possible to declare constraints at ensemble specification
    * time.
    *
    * @param clause Constraint expression
    */
  def constraint(clause: => Logical): Unit =
    _constraintsClauseFuns += clause _

  /** Build a summary constraint from all registered constraint expressions.
    *
    * The single generated constraint can then be posted conditional on ensemble activation.
    */
  private[trust] def _buildConstraintsClause: Logical = {
    if (_constraintsClauseFuns.nonEmpty) _solverModel.and(_constraintsClauseFuns.map(_.apply()))
    else LogicalBoolean(true)
  }
}
