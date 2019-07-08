package scenario.testing

import cz.cuni.mff.d3s.enact.{Ensemble, Policy}

trait Spec[ScenarioType] {
  def makeScenario(): ScenarioType
  def toPerfLine: String
  def policy(scenario: ScenarioType): Policy[_]
  def root(scenario: ScenarioType): Ensemble
}
