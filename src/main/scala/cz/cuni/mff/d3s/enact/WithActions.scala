package cz.cuni.mff.d3s.enact

import scala.collection.mutable

trait WithActions {
  this: WithEnsembleGroups =>

  private[enact] val _actions = mutable.ListBuffer.empty[() => Iterable[Action]]

  private[enact] def _collectActions(): Iterable[Action] = {
    val groupActions = _ensembleGroups.values.flatMap(
      group => group.selectedMembers.flatMap(member => member._collectActions())
    )

    groupActions ++ _actions.flatMap(_())
  }

  def allow(
      subjects: Role[Component],
      action: String,
      objects: Role[Component]
  ): Unit = {
    _actions += (() => {
      for {
        objct <- objects.selectedMembers
        subject <- subjects.selectedMembers
      } yield AllowAction(subject, action, objct)
    })
  }

  def deny(
      subjects: Role[Component],
      action: String,
      objects: Role[Component],
  ): Unit = {
    _actions += (() => {
      for {
        objct <- objects.selectedMembers
        subject <- subjects.selectedMembers
      } yield DenyAction(subject, action, objct)
    })
  }

  def notify(
      subjects: Role[Component],
      notification: Notification,
  ): Unit = {
    _actions += (() => {
      val members = subjects.selectedMembers
      members.foreach(_.notify(notification))
      members.map(NotifyAction(_, notification))
    })
  }
}
