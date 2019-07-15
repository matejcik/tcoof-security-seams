package scenario.lunch

import cz.cuni.mff.d3s.trust.{Component, Ensemble, Policy}

class Person(name: String) extends Component {
  name(name)
}

class SimpleScenario(val people: Seq[Person]) {

  class HelloWorld extends Ensemble {
    val greeter = oneOf(people)

    allow(greeter, "greet", people)
  }

  val problem = Policy.root(new HelloWorld)
}

object SimpleScenario {
  val names = Seq("Roland", "Lilith", "Mordecai", "Brick")

  def main(args: Array[String]): Unit = {
    val people = for (name <- names) yield new Person(name)
    val scn = new SimpleScenario(people)

    scn.problem.resolve()
    for (action <- scn.problem.actions) println(action)
  }
}