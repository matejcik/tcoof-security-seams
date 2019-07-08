package scenario.testing

import tcof._

trait Spec[ScenarioType] {
  def makeScenario(): ScenarioType
  def toPerfLine: String
  def policy(scenario: ScenarioType): Policy[_]
  def root(scenario: ScenarioType): Ensemble
}
