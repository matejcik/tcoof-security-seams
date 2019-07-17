package cz.cuni.mff.d3s.trust

/** Common superclass of possible security actions. */
abstract class Action

/** Allow action.
  *
  * Grants the `actor` permission to perform `action` on the `subject`
  *
  * @param actor Actor component
  * @param action Action name
  * @param subject Action target component
  */
case class AllowAction(actor: Component, action: String, subject: Component) extends Action

/** Deny action.
  *
  * Denies the `actor` permission to perform `action` on the `subject`
  *
  * @param actor Actor component
  * @param action Action name
  * @param subject Action target component
  */
case class DenyAction(actor: Component, action: String, subject: Component) extends Action

/** Notify action.
  *
  * Sends the `target` the specified `notification`.
  *
  * @param target Target component
  * @param notification Notification message
  */
case class NotifyAction(target: Component, notification: Notification) extends Action
