package tcof

import scala.collection.mutable

object PrivacyLevel extends Enumeration {
  val ANY = Value("any")
  val SENSITIVE = Value("sensitive")
  type PrivacyLevel = Value
}

abstract class Action
case class AllowAction(subj: Component, action: String, obj: Component)
    extends Action
case class DenyAction(
    subj: Component,
    action: String,
    obj: Component,
    privacyLevel: PrivacyLevel.PrivacyLevel
) extends Action
case class NotifyAction(subj: Component, notification: Notification)
    extends Action

trait WithActionsInEnsemble {
  this: Ensemble =>

  private[tcof] val _actions = mutable.ListBuffer.empty[() => Iterable[Action]]

  private[tcof] def _collectActions(): Iterable[Action] = {
    val groupActions = _ensembleGroups.values.flatMap(
      group => group.selectedMembers.flatMap(member => member._collectActions())
    )

    groupActions ++ _actions.flatMap(_())
  }

  def allow(subject: Component, action: String, objct: Component): Unit =
    allow(List(subject), action, List(objct))
  def allow(
      subjects: => Iterable[Component],
      action: String,
      objct: Component
  ): Unit = allow(subjects, action, List(objct))
  def allow(
      subject: Component,
      action: String,
      objects: => Iterable[Component]
  ): Unit =
    allow(List(subject), action, objects)

  def allow(
      subjects: => Iterable[Component],
      action: String,
      objects: => Iterable[Component]
  ): Unit = {
    _actions += (() => {
      for {
        objct <- objects
        subject <- subjects
      } yield AllowAction(subject, action, objct)
    })
  }

  def deny(
      subject: Component,
      action: String,
      objct: Component,
      privacyLevel: PrivacyLevel.PrivacyLevel
  ): Unit =
    deny(List(subject), action, List(objct), privacyLevel)
  def deny(
      subjects: => Iterable[Component],
      action: String,
      objct: Component,
      privacyLevel: PrivacyLevel.PrivacyLevel
  ): Unit =
    deny(subjects, action, List(objct), privacyLevel)
  def deny(
      subject: Component,
      action: String,
      objects: => Iterable[Component],
      privacyLevel: PrivacyLevel.PrivacyLevel
  ): Unit =
    deny(List(subject), action, objects, privacyLevel)

  def deny(
      subjects: => Iterable[Component],
      action: String,
      objects: => Iterable[Component],
      privacyLevel: PrivacyLevel.PrivacyLevel
  ): Unit = {
    _actions += (() => {
      for {
        objct <- objects
        subject <- subjects
      } yield DenyAction(subject, action, objct, privacyLevel)
    })
  }

  def notify(subject: Component, notification: Notification): Unit =
    notify(List(subject), notification)

  def notify(
      subjects: => Iterable[Component],
      notification: Notification
  ): Unit = {
    _actions += (() => {
      subjects.foreach(_.notify(notification))
      subjects.map(NotifyAction(_, notification))
    })
  }
}
