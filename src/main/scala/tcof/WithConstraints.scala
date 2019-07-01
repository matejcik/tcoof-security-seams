package tcof

import scala.collection.mutable

trait WithConstraints extends Initializable {
  private[tcof] val _constraintsClauseFuns =
    mutable.ListBuffer.empty[() => Logical]

  def constraint(clause: => Logical): Unit = {
    _constraintsClauseFuns += clause _
  }

  private[tcof] def _buildConstraintsClause: Logical = {
    if (_constraintsClauseFuns.nonEmpty)
      _solverModel.and(_constraintsClauseFuns.map(_.apply()))
    else
      LogicalBoolean(true)
  }
}
