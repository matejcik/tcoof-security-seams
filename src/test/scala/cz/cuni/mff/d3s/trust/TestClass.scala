package cz.cuni.mff.d3s.trust

import org.scalatest.{FlatSpec, Matchers}

trait TestClass extends FlatSpec with Matchers {
  case class Member(id: Int) extends Component {
    override def toString: String = s"Member:$id"
  }
}
