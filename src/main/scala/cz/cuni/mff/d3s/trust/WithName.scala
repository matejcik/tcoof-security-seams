package cz.cuni.mff.d3s.trust

trait WithName {
  private[trust] var _name = Utils.randomName

  def name(nm: String): Unit = _name = nm
  def name: String = _name
}
