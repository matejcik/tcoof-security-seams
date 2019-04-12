package scenario

import java.io.{File, PrintWriter}
import java.time.LocalDate
import java.time.format.DateTimeFormatter

import scenario.model._
import scenario.model.ScenarioSpec._
import org.chocosolver.solver.search.loop.lns.neighbors.Neighbor
import org.chocosolver.util.tools.TimeUtils

import scala.util.Random
import scala.util.control.Breaks._

object TestScenario {
  val TEST_ROUNDS = 100
  val SOLVER_TIME_LIMIT = 30L * 1000

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

      measurements.exists(
        _ < SOLVER_TIME_LIMIT * TimeUtils.MILLISECONDS_IN_NANOSECONDS
      )
    }

    log(s"===== $description =====")
    loop(singleRun)
    perfLogWriter.close()
  }

  def solutionFitsAllWorkers(model: LunchModel): Boolean = {
    val hungryWorkers = model.workers.filter(_.hungry)
    val alreadyNotified =
      hungryWorkers.count(_.notified[RoomAssignedNotification])
    val selectCardinalities =
      model.problem.instance.lunchroomAssignments.selectedMembers
        .map(_.assignees.cardinality.solutionValue)
        .sum
    alreadyNotified + selectCardinalities == hungryWorkers.length
  }

  def solveUntilFitsAll(spec: ScenarioSpec): Measure = {
    val model = ModelGenerator.modelFromSpec(spec)

    val start = System.nanoTime()
    model.problem.init()
    model.problem.solverLimitTime(SOLVER_TIME_LIMIT)
    //    val init = System.nanoTime()
    while (model.problem.solve() && !solutionFitsAllWorkers(model)) {}
    if (model.problem.exists) {
      model.problem.commit()
      //      for (action <- model.problem.actions) println(action)
    }
    val end = System.nanoTime()
    val time = end - start

    val success = time < SOLVER_TIME_LIMIT * TimeUtils.MILLISECONDS_IN_NANOSECONDS

    Measure(success, time, model.problem.instance.solutionUtility)
  }

  def solveScenario(spec: ScenarioSpec): Measure = {
    val model = ModelGenerator.modelFromSpec(spec)

    val start = System.nanoTime()
    model.problem.init()
    model.problem.solverLimitTime(SOLVER_TIME_LIMIT)
    while (model.problem.solve()) {}
    if (model.problem.exists) {
      model.problem.commit()
//      for (action <- model.problem.actions) println(action)
    }
//    println(model.problem.instance.toStringWithUtility)
    val end = System.nanoTime()
    val time = end - start

    val success = time < SOLVER_TIME_LIMIT * TimeUtils.MILLISECONDS_IN_NANOSECONDS

    Measure(success, time, model.problem.instance.solutionUtility)
  }

  def solveOneByOne(spec: ScenarioSpec): Measure = {
    val model = ModelGenerator.modelFromSpec(spec)

    val hungryWorkers = Random.shuffle(model.workers.filter(_.hungry))
    hungryWorkers.foreach(_.hungry = false)

    val start = System.nanoTime()

    breakable {
      for (worker <- hungryWorkers) {
        worker.hungry = true

        model.problem.init()
        model.problem.solverLimitTime(SOLVER_TIME_LIMIT)
        while (model.problem.solve()) {}
        if (model.problem.exists) {
          model.problem.commit()
          //for (action <- model.problem.actions) println(action)
        }

        if (System.nanoTime() - start > SOLVER_TIME_LIMIT) break
      }
    }
    //    println(model.problem.instance.toStringWithUtility)
    val end = System.nanoTime()
    val time = end - start

    val success =
      model.problem.exists &&
      (time < SOLVER_TIME_LIMIT * TimeUtils.MILLISECONDS_IN_NANOSECONDS)
    val utility = if (success) model.problem.instance.solutionUtility else 0
    Measure(success, time, utility)
  }

  def warmup: Unit = {
    val spec = ScenarioSpec(
      projects = 3,
      lunchrooms = (3, 10),
      workrooms = (3, 10),
      workers = 10,
      hungryWorkers = 10,
      fillRooms = false,
      isLunchTime = true,
    )
    warmup(spec)
  }

  def warmup(spec: ScenarioSpec,
             solverFunc: ScenarioSpec => Measure = solveScenario): Unit = {
    var totalTime: Long = 0
    while (totalTime < 10L * 1000 * 1000 * 1000) {
      totalTime += solverFunc(spec).time
    }
    log(s"warmup completed in ${formatMs(totalTime)} ms")
  }

  def measure_workerCount_simple =
    measure("workercount-simple", "varying worker count - not lunch hour") {
      m =>
        val defaultSpec = ScenarioSpec(
          projects = 40,
          lunchrooms = (0, 0),
          workrooms = (100, 50),
          workers = 50,
          hungryWorkers = 0,
          fillRooms = false,
          isLunchTime = false,
        )
        warmup(defaultSpec)
        for (projectCount <- Seq(5, 15, 50)) {
          breakable {
            for (workerCount <- 100.to(1000, 100)) {
              val spec = defaultSpec.copy(
                projects = projectCount,
                workers = workerCount,
              )
              if (!m(spec)) break
            }
          }
        }
    }

  def measure_moreProjectsThanRooms_compareMethods = {
    val defaultSpec = ScenarioSpec(
      projects = 7,
      lunchrooms = (2, 5),
      workrooms = (10, 50),
      workers = 50,
      hungryWorkers = 5,
      fillRooms = false,
      isLunchTime = true,
    )
    val measuringLoop = (m: ScenarioSpec => Boolean) =>
      for (lunchroomCount <- 2 to 5) {
        breakable {
          for (workerCount <- 5 to 40) {
            val spec = defaultSpec.copy(
              hungryWorkers = workerCount,
              lunchrooms = (lunchroomCount, 5)
            )
            if (!m(spec)) break
          }
        }
      }

    measure(
      "moreprojects-optimizing",
      "more projects than roooms - optimizing solver",
      solverFunc = solveScenario
    ) { m =>
      warmup(defaultSpec)
      measuringLoop(m)
    }

    measure(
      "moreprojects-satisfying",
      "more projects than roooms - satisfying solver",
      solverFunc = solveUntilFitsAll
    ) { m =>
      warmup(defaultSpec, solverFunc = solveUntilFitsAll)
      measuringLoop(m)
    }

    measure(
      "moreprojects-onebyone",
      "more projects than roooms - one-by-one assignment",
      solverFunc = solveOneByOne
    ) { m =>
      warmup(defaultSpec, solverFunc = solveOneByOne)
      measuringLoop(m)
    }
  }

  def measure_moreRoomsThanProjects_compareMethods = {
    val defaultSpec = ScenarioSpec(
      projects = 3,
      lunchrooms = (4, 10),
      workrooms = (10, 50),
      workers = 50,
      hungryWorkers = 5,
      fillRooms = false,
      isLunchTime = true,
    )
    val measuringLoop = (m: ScenarioSpec => Boolean) => breakable {
      for (workerCount <- 5 to 40) {
        val spec = defaultSpec.copy(hungryWorkers = workerCount)
        if (!m(spec)) break
      }
    }

    measure(
      "morerooms-optimizing",
      "more rooms than projects - optimizing solver",
      solverFunc = solveScenario
    ) { m =>
      warmup(defaultSpec)
      measuringLoop(m)
    }

    measure(
      "morerooms-satisfying",
      "more rooms than projects - satisfying solver",
      solverFunc = solveUntilFitsAll
    ) { m =>
      warmup(defaultSpec, solverFunc = solveUntilFitsAll)
      measuringLoop(m)
    }

    measure(
      "morerooms-onebyone",
      "more rooms than projects - one-by-one assignment",
      solverFunc = solveOneByOne
    ) { m =>
      warmup(defaultSpec, solverFunc = solveOneByOne)
      measuringLoop(m)
    }
  }

  def measure_oneByOne_growingNumberOfProjects =
    measure(
      "onebyone-projects",
      "varying worker count with growing number of projects",
      solverFunc = solveOneByOne
    ) { m =>
      val defaultSpec = ScenarioSpec(
        projects = 20,
        lunchrooms = (20, 100),
        workrooms = (10, 50),
        workers = 5000,
        hungryWorkers = 1,
        fillRooms = false,
        isLunchTime = true,
      )
      warmup(defaultSpec, solverFunc = solveOneByOne)

      for (fillRooms <- Seq(false, true)) {
        for (projectCount <- 20.to(100, 5)) {
          val spec = defaultSpec.copy(
            lunchrooms = (projectCount, 100),
            fillRooms = fillRooms,
          )
          m(spec)
        }
      }
    }

  def main(args: Array[String]): Unit = {
//    val defaultSpec = ScenarioSpec(
//      projects = 3,
//      lunchrooms = (15, 5),
//      workrooms = (10, 50),
//      workers = 200,
//      hungryWorkers = 5,
//      preassignedRooms = 0,
//      isLunchTime = true,
//    )
//    warmup(defaultSpec, 10, solveOneByOne)
//    return
    measure_workerCount_simple
    measure_moreRoomsThanProjects_compareMethods
    measure_oneByOne_growingNumberOfProjects
  }
}
