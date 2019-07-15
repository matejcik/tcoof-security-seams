package cz.cuni.mff.d3s.trust

abstract class Action
case class AllowAction(subj: Component, action: String, obj: Component)
  extends Action
case class DenyAction(subj: Component, action: String, obj: Component)
  extends Action
case class NotifyAction(subj: Component, notification: Notification)
  extends Action