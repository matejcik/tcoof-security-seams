package cz.cuni.mff.d3s.trust

object InitStages extends Enumeration {
  type InitStages = Value

  val ConfigPropagation, VarsCreation, RulesCreation = Value
}
