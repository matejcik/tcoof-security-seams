package cz.cuni.mff.d3s.trust

import scala.collection.mutable

/** Ensemble part for handling security actions.
  *
  * Provides DSL functions that create security actions and notifications, and
  * functionality for collecting the actions from sub-ensembles.
  */
trait WithActions {
  this: WithEnsembleGroups =>

  /** Local type alias for roles. */
  private type Role = MemberGroup[Component]

  /** List of generators of actions. */
  private[trust] val _actions = mutable.ListBuffer.empty[() => Iterable[Action]]

  /** Collect security actions from this ensemble and all its sub-ensembles. */
  private[trust] def _collectActions(): Iterable[Action] = {
    val groupActions = _ensembleGroups
      .flatMap(_.selectedMembers)
      .flatMap(_._collectActions())

    groupActions ++ _actions.flatMap(_())
  }

  /** Generate an allow action.
    *
    * In place of `actors` and `subjects`, it is possible to use a role object,
    * a component, or a collection of components. Implicit conversions from components
    * to roles are invoked if the argument is not a role object.
    *
    * @param actors Actor role
    * @param action Action name
    * @param subjects Subjects role
    */
  def allow(actors: Role, action: String, subjects: Role): Unit = {
    _actions += (() => {
      for {
        subject <- subjects.selectedMembers
        actor <- actors.selectedMembers
      } yield AllowAction(actor, action, subject)
    })
  }

  /** Generate a deny action.
    *
    * In place of `actors` and `subjects`, it is possible to use a role object,
    * a component, or a collection of components. Implicit conversions from components
    * to roles are invoked if the argument is not a role object.
    *
    * @param actors Actor role
    * @param action Action name
    * @param subjects Subjects role
    */
  def deny(actors: Role, action: String, subjects: Role): Unit = {
    _actions += (() => {
      for {
        subject <- subjects.selectedMembers
        actor <- actors.selectedMembers
      } yield DenyAction(actor, action, subject)
    })
  }

  /** Generate a notify action
    *
    * In place of `target` and `subjects`, it is possible to use a role object,
    * a component, or a collection of components. Implicit conversions from components
    * to roles are invoked if the argument is not a role object.
    *
    * When the security actions are collected with [[Policy.commit]], notifications
    * will also be sent to component instances.
    *
    * @param targets Target role
    * @param notification Notification message
    */
  def notify(targets: Role, notification: Notification): Unit = {
    _actions += (() => {
      val members = targets.selectedMembers
      members.foreach(_.notify(notification))
      members.map(NotifyAction(_, notification))
    })
  }
}
