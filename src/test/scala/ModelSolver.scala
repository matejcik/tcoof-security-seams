import tcof._

trait ModelSolver {
  case class Member(id: Int) extends Component {
    override def toString: String = s"Member:$id"
  }

  def root[T <: RootEnsemble](builder: => T): RootEnsembleAnchor[T] =
    new RootEnsembleAnchor(builder _)
}
