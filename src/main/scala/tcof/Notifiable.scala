package tcof
import collection.mutable
import collection.immutable.Traversable

trait Notification

trait Notifiable {
  private[tcof] val _notificationsReceived = mutable.Set.empty[Notification]

  def notified[T <: Notification]: Boolean = _notificationsReceived.exists(_.isInstanceOf[T])
  def notified(notification: Notification): Boolean = _notificationsReceived.contains(notification)
  def notify(notification: Notification): Unit = _notificationsReceived += notification

  def notifications: Traversable[Notification] = _notificationsReceived.to[Traversable]
  def clearNotification(notification: Notification): Unit = _notificationsReceived -= notification
}
