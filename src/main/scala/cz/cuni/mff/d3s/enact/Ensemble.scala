package cz.cuni.mff.d3s.enact

import Utils._
import scala.language.implicitConversions

trait Ensemble
    extends Initializable
    with WithName
    with WithUtility
    with WithSelectionStatus
    with WithEnsembleGroups
    with WithRoles
    with WithActions
    with WithConstraints
    with CommonImplicits {

  private[enact] var _situationFun: () => Boolean = null

  def situation(cond: => Boolean): Unit = {
    _situationFun = cond _
  }

  private[enact] def _isInSituation: Boolean = {
    if (_situationFun != null)
      _situationFun()
    else
      true
  }

  override def toString: String = s"<Ensemble:$name>"

  implicit def ensembleGroupToMembers[E <: Ensemble](group: EnsembleGroup[E]): Iterable[E] =
    group.allMembers
}
