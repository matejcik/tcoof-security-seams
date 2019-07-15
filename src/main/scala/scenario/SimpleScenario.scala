package scenario

import cz.cuni.mff.d3s.trust.{Component, Ensemble, Policy}

class Person(name: String) extends Component {
  name(name)
}

class SimpleScenario(val people: Seq[Person]) {

  class HelloWorld extends Ensemble {
    val greeter = oneOf(people)

    allow(greeter, "greet", people)
  }

  val policy = Policy.root(new HelloWorld)
}

object SimpleScenario {
  val names = Seq("Roland", "Lilith", "Mordecai", "Brick")

  def main(args: Array[String]): Unit = {
    val people = for (name <- names) yield new Person(name)
    val scenario = new SimpleScenario(people)

    scenario.policy.resolve()
    for (action <- scenario.policy.actions) println(action)
  }
}