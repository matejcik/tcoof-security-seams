package scenario.testing

import cz.cuni.mff.d3s.trust.Policy

/** Test case spec.
  *
  * @tparam ScenarioType Scenario type which is managed by the spec.
  */
trait Spec[ScenarioType] {
  this: Product =>

  /** Create a new instance of the scenario. */
  def makeScenario(): ScenarioType

  /** Comma-separated representation of parameters in the spec. */
  def toPerfLine: String = productIterator.mkString(", ")

  /** Return the policy object as an instance of Policy[anything].
    *
    * Necessary because [[cz.cuni.mff.d3s.trust.Policy]] type parameter is invariant
    * and cannot be made covariant, so the test harness could not access it without
    * knowing the concrete root ensemble type.
    */
  def policy(scenario: ScenarioType): Policy[_]
}
