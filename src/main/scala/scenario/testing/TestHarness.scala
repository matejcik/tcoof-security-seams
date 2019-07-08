package scenario.testing

import java.io.{File, PrintWriter}
import java.time.LocalDate
import java.time.format.DateTimeFormatter

import org.chocosolver.util.tools.TimeUtils

class TestHarness[ScenarioType] {
  type ScenarioSpec = Spec[ScenarioType]

  val TEST_ROUNDS = 50
  val SOLVER_TIME_LIMIT = 30L * 1000
  val LIMIT_NANO = SOLVER_TIME_LIMIT * TimeUtils.MILLISECONDS_IN_NANOSECONDS

  val RESULT_PATH = "results/" + LocalDate.now.format(
    DateTimeFormatter.ofPattern("YYYY-MM-dd")
  )
  new File(RESULT_PATH).mkdir()

  val logWriter = new PrintWriter(new File("lunch.log"))

  def log(s: String): Unit = synchronized {
    println(s)
    logWriter.println(s)
    logWriter.flush()
  }

  case class Measure(success: Boolean, time: Long, utility: Int)

  def formatMs(nanosec: Long): String = f"${nanosec.toDouble / 1000000}%.05f"

  def measure(
      label: String,
      description: String,
      solverFunc: ScenarioSpec => Measure = solveScenario
  )(loop: (ScenarioSpec => Boolean) => Unit): Unit = {
    val perfLogWriter = new PrintWriter(new File(s"$RESULT_PATH/$label.log"))

    def perf(spec: ScenarioSpec, runIndex: Int, success: Boolean, time: Long) {
      perfLogWriter.println(s"${spec.toPerfLine}, $runIndex, $time")
      perfLogWriter.flush()
    }

    def singleRun(spec: ScenarioSpec): Boolean = {
      var utility = 0
      val measurements = for (i <- 0 until TEST_ROUNDS) yield {
        val m = solverFunc(spec)
        perf(spec, i, m.success, m.time)
        utility = m.utility
        m.time
      }

      val measurementsSorted = measurements.sorted
      val min = formatMs(measurements.min)
      val max = formatMs(measurements.max)
      val avg = formatMs(measurements.sum / TEST_ROUNDS)
      val med = formatMs(measurementsSorted(TEST_ROUNDS / 2))
      log(
        s"Scenario ${spec} solved in avg $avg ms "
          + s" (min: $min, max: $max, med: $med), utility $utility"
      )

      measurements.exists(_ < LIMIT_NANO)
    }

    log(s"===== $description =====")
    loop(singleRun)
    perfLogWriter.close()
  }

  def solveScenario(spec: ScenarioSpec): Measure = {
    val model = spec.makeScenario()
    val policy = spec.policy(model)

    val start = System.nanoTime()
    policy.init()
    policy.solverLimitTime(SOLVER_TIME_LIMIT)
    while (policy.solve()) {}
    if (policy.exists) {
      policy.commit()
      //      for (action <- model.problem.actions) println(action)
    }
    //    println(model.problem.instance.toStringWithUtility)
    val end = System.nanoTime()
    val time = end - start

    val success = time < LIMIT_NANO
    Measure(success, time, spec.root(model).solutionUtility)
  }
}