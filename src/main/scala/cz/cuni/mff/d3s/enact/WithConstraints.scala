package cz.cuni.mff.d3s.enact

import scala.collection.mutable

trait WithConstraints extends Initializable {
  private[enact] val _constraintsClauseFuns =
    mutable.ListBuffer.empty[() => Logical]

  def constraint(clause: => Logical): Unit = {
    _constraintsClauseFuns += clause _
  }

  private[enact] def _buildConstraintsClause: Logical = {
    if (_constraintsClauseFuns.nonEmpty)
      _solverModel.and(_constraintsClauseFuns.map(_.apply()))
    else
      LogicalBoolean(true)
  }
}
