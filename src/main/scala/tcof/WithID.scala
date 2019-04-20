package tcof

trait WithID {
  type IDType

  def id: IDType
}
