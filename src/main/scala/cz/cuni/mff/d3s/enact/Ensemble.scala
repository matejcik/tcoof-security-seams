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

  def _collectUtility: Option[Integer] = {
    // TODO should this be in WithUtility?
    val noMemberHasUtility = _ensembleGroups.values
      .flatMap(g => g.allMembers.flatMap(_._collectUtility))
      .isEmpty
    if (noMemberHasUtility) {
      _getUtility
    } else {
      val subUtilities = _ensembleGroups.values.map(
        g => g.sum(_._collectUtility.getOrElse(_solverModel.IntegerInt(0)))
      )
      Some(
        utility + subUtilities
          .reduceOption(_ + _)
          .getOrElse(_solverModel.IntegerInt(0))
      )
    }
  }

  override def toString: String =
    s"""Ensemble "$name":\n${indent("rOles:\n" + _roles.values.mkString(""), 1)}${indent(
      "groups:\n" + _ensembleGroups.mkString(""),
      1
    )}"""

  def toStringWithUtility: String = {
    s"""Ensemble "$name" (utility: $solutionUtility):\n${indent(
      _roles.values.mkString(""),
      1
    )}${indent(_ensembleGroups.mapValues(_.toStringWithUtility).mkString(""), 1)}\n"""
  }

  implicit def ensembleGroupToMembers[E <: Ensemble](
      group: EnsembleGroup[E]
  ): Iterable[E] = group.allMembers
}
