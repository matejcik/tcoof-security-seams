# TCOOF-Trust

Ensemble-based access control framework.

Define your security policy like this:

```scala
class SimpleScenario(val people: Seq[Person]) {

  class HelloWorld extends Ensemble {
    val greeter = oneOf(people)

    allow(greeter, "greet", people)
  }

  val policy = Policy.root(new HelloWorld)
}
```

Find the solution and query access rights:

```scala
val names = Seq("Roland", "Lilith", "Mordecai", "Brick")
val people = for (name <- names) yield new Person(name)
val scenario = new SimpleScenario(people)

val roland = people(0)
val lilith = people(1)
scenario.policy.resolve()
if (scenario.policy.allows(roland, "greet", lilith)) {
  println("Roland says: Hello, Lilith!")
}
```

## Requirements

* [Scala 2.12](https://www.scala-lang.org/download/2.12.8.html)
* [sbt](https://www.scala-sbt.org/)

For generating graphs:

* [Python 3.6](https://www.python.org/)
* [pipenv](https://github.com/pypa/pipenv)

Graphing scripts should work with any Python later than 3.6, but the requirements are
locked to 3.6. You might need to modify `Pipfile`.

## Usage

To run the example program:

    $ sbt run
   
To run the measurement suite:

    $ sbt "run scenario.MeasureAll"
    
Results will be saved in `results/YYYY-MM-DD` directory.

To generate graphs from measurements saved in `results/final`:

    $ cd python
    $ ./all.sh
    

## Including in your project

Generate the JAR with:

    $ sbt package
    
Then copy `target/scala-2.12/tcoof-trust_2.12-1.0.jar` into a `lib/` subdirectory of
your project.
    
    
## Documentation

To generate the API documentation:

    $ sbt doc

Documentation is saved to `target/scala-2.12/api`.

You can find the user guide and implementation notes in the [thesis](thesis/thesis.pdf).


## Development

Install [IntelliJ IDEA](https://www.jetbrains.com/idea/) with Scala plugin, and open
this repository as a Scala project.