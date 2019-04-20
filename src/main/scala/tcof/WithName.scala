package tcof

trait WithName {
  private[tcof] var _name = Utils.randomName

  def name(nm: String): Unit = _name = nm
  def name: String = _name
}
