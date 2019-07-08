package cz.cuni.mff.d3s.enact

import scala.collection.mutable

trait WithActions {
  this: WithEnsembleGroups =>

  private type Role = MemberGroup[Component]

  private[enact] val _actions = mutable.ListBuffer.empty[() => Iterable[Action]]

  private[enact] def _collectActions(): Iterable[Action] = {
    val groupActions = _ensembleGroups
      .flatMap(_.selectedMembers)
      .flatMap(_._collectActions())

    groupActions ++ _actions.flatMap(_())
  }

  def allow(subjects: Role, action: String, objects: Role): Unit = {
    _actions += (() => {
      for {
        objct <- objects.selectedMembers
        subject <- subjects.selectedMembers
      } yield AllowAction(subject, action, objct)
    })
  }

  def deny(subjects: Role, action: String, objects: Role): Unit = {
    _actions += (() => {
      for {
        objct <- objects.selectedMembers
        subject <- subjects.selectedMembers
      } yield DenyAction(subject, action, objct)
    })
  }

  def notify(subjects: Role, notification: Notification): Unit = {
    _actions += (() => {
      val members = subjects.selectedMembers
      members.foreach(_.notify(notification))
      members.map(NotifyAction(_, notification))
    })
  }
}
