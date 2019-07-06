package tcof

import org.chocosolver.solver.Solution

class IntegerTest extends ModelSolver {

  class IntegerVsInt(
      left: Integer,
      right: Integer,
      eq: Integer,
      less: Integer,
      more: Integer
  ) {
    val add = left + right
    val sub = left - right
    val mul = left * right
    val div = left / right

    val constraints = Seq(
      left === eq,
      left != less,
      left < more,
      left <= eq,
      left > less,
      left >= eq
    )
  }

  def testCase(
      model: SolverModel,
      makeLeft: Int => Integer,
      makeRight: Int => Integer
  ) = {
    model.init()
    val left = makeLeft(10)
    val right = makeRight(5)
    val eq = makeRight(10)
    val less = makeRight(5)
    val more = makeRight(15)

    val test = new IntegerVsInt(left, right, eq, less, more)

    model.post(model.and(test.constraints))
    assert(model.solveAndRecord())
    assert(!model.solveAndRecord())
    assert(model.exists)

    test.add.asInt shouldEqual 15
    test.sub.asInt shouldEqual 5
    test.mul.asInt shouldEqual 50
    test.div.asInt shouldEqual 2
  }

  "IntegerIntVar" should "work with right-hand Int" in {
    val model = new SolverModel
    def intvar(x: Int) = model.IntegerIntVar(model.intVar(x))
    def int(x: Int) = model.IntegerInt(x)
    testCase(model, intvar, int)
  }

  it should "work with left-hand Int" in {
    val model = new SolverModel
    def intvar(x: Int) = model.IntegerIntVar(model.intVar(x))
    def int(x: Int) = model.IntegerInt(x)
    testCase(model, int, intvar)
  }

  it should "work on both sides" in {
    val model = new SolverModel
    def intvar(x: Int) = model.IntegerIntVar(model.intVar(x))
    def int(x: Int) = model.IntegerInt(x)
    testCase(model, intvar, intvar)
  }

  "IntegerInt" should "also work on both sides" in {
    val model = new SolverModel
    def intvar(x: Int) = model.IntegerIntVar(model.intVar(x))
    def int(x: Int) = model.IntegerInt(x)
    testCase(model, int, int)
  }
}
