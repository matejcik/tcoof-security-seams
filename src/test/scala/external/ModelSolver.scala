package external

import cz.cuni.mff.d3s.enact.Component
import org.scalatest.{FlatSpec, Matchers}

trait ModelSolver extends FlatSpec with Matchers {
  case class Member(id: Int) extends Component {
    override def toString: String = s"Member:$id"
  }
}
