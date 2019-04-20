package tcof

abstract class Model {
  private var _universe = Seq.empty[Component]
  def components_=(univ: Seq[Component]): Unit = _universe = univ
  def components: Seq[Component] = _universe

  protected def root[EnsembleType <: RootEnsemble](
      builder: => EnsembleType
  ): RootEnsembleAnchor[EnsembleType] = {
    new RootEnsembleAnchor(builder _)
  }
}
