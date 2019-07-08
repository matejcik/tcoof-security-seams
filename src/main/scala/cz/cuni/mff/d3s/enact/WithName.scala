package cz.cuni.mff.d3s.enact

trait WithName {
  private[enact] var _name = Utils.randomName

  def name(nm: String): Unit = _name = nm
  def name: String = _name
}
