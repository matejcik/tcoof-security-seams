package newcase

import java.io.{File, PrintWriter}
import java.time.LocalDate
import java.time.format.DateTimeFormatter

import newcase.model._
import newcase.model.ScenarioSpec._
import org.chocosolver.solver.search.loop.lns.neighbors.Neighbor
import org.chocosolver.util.tools.TimeUtils

import scala.util.Random
import scala.util.control.Breaks._

object TestScenario {
  val TEST_ROUNDS = 5
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
      val perfOrFail = if (success) time.toString else "FAIL"
      perfLogWriter.println(s"${spec.toPerfLine}, $runIndex, $perfOrFail")
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

        if (solutionFitsAllWorkers(model)) {
          break
        }
      }
    }
    //    println(model.problem.instance.toStringWithUtility)
    val end = System.nanoTime()
    val time = end - start

    val success = time < SOLVER_TIME_LIMIT * TimeUtils.MILLISECONDS_IN_NANOSECONDS

    Measure(success, time, model.problem.instance.solutionUtility)
  }

  def warmup: Unit = {
    val spec = ScenarioSpec(
      projects = 3,
      lunchrooms = (3, 10),
      workrooms = (3, 10),
      workers = 10,
      hungryWorkers = 10,
      preassignedRooms = 0,
      isLunchTime = true
    )
    warmup(spec)
  }

  def warmup(spec: ScenarioSpec,
             repeats: Int = 100,
             solverFunc: ScenarioSpec => Measure = solveScenario): Unit = {
    var totalTime: Long = 0
    for (_ <- 0 until repeats) {
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
          preassignedRooms = 0,
          isLunchTime = false,
        )
        warmup(defaultSpec, 100000)
        breakable {
          for (workerCount <- 1000.to(30000, 1000)) {
            val spec = defaultSpec.copy(workers = workerCount)
            if (!m(spec)) break
          }
        }
    }

  def measure_workerCount_moreProjectsThanRooms =
    measure(
      "more-projects-than-rooms",
      "varying worker count - more projects than rooms"
    ) { m =>
      val defaultSpec = ScenarioSpec(
        projects = 7,
        lunchrooms = (2, 5),
        workrooms = (10, 50),
        workers = 50,
        hungryWorkers = 5,
        preassignedRooms = 0,
        isLunchTime = true,
      )
      warmup(defaultSpec)
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
    }

  def measure_moreRoomsThanProjects(sat: Boolean) =
    measure(
      "more-rooms-than-projects" + (if (sat) "-sat" else ""),
      "varying worker count - more rooms than projects"
        + (if (sat) " (satisfy)" else ""),
      solverFunc = if (sat) solveUntilFitsAll _ else solveScenario _
    ) { m =>
      val defaultSpec = ScenarioSpec(
        projects = 3,
        lunchrooms = (4, 10),
        workrooms = (10, 50),
        workers = 50,
        hungryWorkers = 5,
        preassignedRooms = 0,
        isLunchTime = true,
      )
      warmup(defaultSpec)
      breakable {
        for (workerCount <- 5 to 40) {
          val spec = defaultSpec.copy(hungryWorkers = workerCount)
          if (!m(spec)) break
        }
      }
    }

  def measure_allVsOneByOne = {
    val defaultSpec = ScenarioSpec(
      projects = 3,
      lunchrooms = (4, 10),
      workrooms = (10, 50),
      workers = 50,
      hungryWorkers = 5,
      preassignedRooms = 0,
      isLunchTime = true,
    )
    warmup(defaultSpec)
    measure(
      "allone-all",
      "all-vs-one - place all workers in the same iteration",
    ) { m =>
      breakable {
        for (workerCount <- 5 to 40) {
          val spec = defaultSpec.copy(hungryWorkers = workerCount)
          if (!m(spec)) break
        }
      }
    }
    warmup(defaultSpec, 1000, solveOneByOne)
    measure(
      "allone-one",
      "all-vs-one - place one worker at a time",
      solverFunc = solveOneByOne,
    ) { m =>
      breakable {
        for (workerCount <- 5 to 40) {
          val spec = defaultSpec.copy(hungryWorkers = workerCount)
          if (!m(spec)) break
        }
      }
    }
  }

  def measure_oneByOne_growingNumberOfProjects =
    measure(
      "onebyone-projects",
      "varying worker count with growing number of projects",
      solverFunc = solveOneByOne
    ) { m =>
      val defaultSpec = ScenarioSpec(
        projects = 3,
        lunchrooms = (15, 20),
        workrooms = (10, 50),
        workers = 500,
        hungryWorkers = 5,
        preassignedRooms = 0,
        isLunchTime = true,
      )
      warmup(defaultSpec, solverFunc = solveOneByOne)
      for (projectCount <- Seq(5, 15, 25)) {
        breakable {
          for (workerCount <- 50.to(500, 50)) {
            val spec = defaultSpec.copy(
              hungryWorkers = workerCount,
              projects = projectCount,
            )
            if (!m(spec)) break
          }
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
    warmup
    measure_oneByOne_growingNumberOfProjects
    //measure_moreRoomsThanProjects(true)
    //measure_moreRoomsThanProjects(false)
    //measure_workerCount_simple
  }
}
