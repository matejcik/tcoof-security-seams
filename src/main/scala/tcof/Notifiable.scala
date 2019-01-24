package tcof
import collection.mutable

trait Notification

trait Notifiable {
  private[tcof] val _notificationsReceived = mutable.Set.empty[Notification]

  def notified(notification: Notification): Boolean = _notificationsReceived.contains(notification)
  def notify(notification: Notification): Unit = _notificationsReceived += notification
}
