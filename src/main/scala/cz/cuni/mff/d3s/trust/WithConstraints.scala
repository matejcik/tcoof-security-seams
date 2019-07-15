package cz.cuni.mff.d3s.trust

import scala.collection.mutable

trait WithConstraints extends Initializable {
  private[trust] val _constraintsClauseFuns =
    mutable.ListBuffer.empty[() => Logical]

  def constraint(clause: => Logical): Unit = {
    _constraintsClauseFuns += clause _
  }

  private[trust] def _buildConstraintsClause: Logical = {
    if (_constraintsClauseFuns.nonEmpty)
      _solverModel.and(_constraintsClauseFuns.map(_.apply()))
    else
      LogicalBoolean(true)
  }
}
