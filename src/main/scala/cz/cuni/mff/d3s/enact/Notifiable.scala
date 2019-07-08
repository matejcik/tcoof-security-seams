package cz.cuni.mff.d3s.enact

import scala.collection.immutable.Traversable
import scala.collection.mutable
import scala.reflect.ClassTag

trait Notification

trait Notifiable {
  private[enact] val _notificationsReceived = mutable.Set.empty[Notification]

  def notified[T <: Notification](implicit tag: ClassTag[T]): Boolean =
    _notificationsReceived.exists(tag.runtimeClass.isInstance(_))
  def notified(notification: Notification): Boolean =
    _notificationsReceived.contains(notification)
  def notify(notification: Notification): Unit =
    _notificationsReceived += notification

  def notifications: Traversable[Notification] =
    _notificationsReceived.to[Traversable]
  def clearNotification(notification: Notification): Unit =
    _notificationsReceived -= notification
}
