package cz.cuni.mff.d3s.trust

import scala.collection.immutable.Traversable
import scala.collection.mutable
import scala.reflect.ClassTag

/** Marker for notification types. */
trait Notification

/** Object that can receive notifications. */
trait Notifiable {

  /** Internal list of all notification messages received by this component. */
  private[trust] val _notificationsReceived = mutable.Set.empty[Notification]

  /** True if any notification of this type was received. */
  def notified[T <: Notification](implicit tag: ClassTag[T]): Boolean =
    _notificationsReceived.exists(tag.runtimeClass.isInstance(_))

  /** True if this particular notification was received. */
  def notified(notification: Notification): Boolean =
    _notificationsReceived.contains(notification)

  /** List of all received notifications.
    *
    * Provides a public interface that disallows modification of the internal list.
    */
  def notifications: Traversable[Notification] =
    _notificationsReceived.to[Traversable]

  /** Send a notification to the component. */
  def notify(notification: Notification): Unit =
    _notificationsReceived += notification

  /** Remove a previously sent notification. */
  def clearNotification(notification: Notification): Unit =
    _notificationsReceived -= notification
}
