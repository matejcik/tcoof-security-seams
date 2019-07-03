import org.scalatest.{FlatSpec, Matchers}
import tcof._

trait ModelSolver extends FlatSpec with Matchers {
  case class Member(id: Int) extends Component {
    override def toString: String = s"Member:$id"
  }
}
