package cz.cuni.mff.d3s.trust

/** Initialization stages.
  *
  * See thesis section 6.2.6 for detailed description.
  */
private[trust] object InitStages extends Enumeration {
  type InitStages = Value

  /** Initialization stages.
    *
    * @see [[Initializable._init()]]
    */
  val ConfigPropagation, VarsCreation, RulesCreation = Value
}
