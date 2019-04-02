package newcase

import java.io.{File, PrintWriter}

import newcase.model._
import newcase.model.ScenarioSpec._
import org.chocosolver.solver.search.loop.lns.neighbors.Neighbor
import org.chocosolver.util.tools.TimeUtils

import scala.util.Random
import scala.util.control.Breaks._

object TestScenario {
  val TEST_ROUNDS = 50
  val SOLVER_TIME_LIMIT = 30L * 1000

  val logWriter = new PrintWriter(new File("lunch.log"))
  val perfLogWriter = new PrintWriter(new File("lunch-perf.log"))

  def log(s: String): Unit = synchronized {
    println(s)
    logWriter.println(s)
    logWriter.flush()
  }

  def perf(spec: ScenarioSpec, runIndex: Int, success: Boolean, time: Long): Unit =
    synchronized {
      val perfOrFail = if (success) time.toString else "FAIL"
      perfLogWriter.println(s"${spec.toPerfLine}, $runIndex, $perfOrFail")
      perfLogWriter.flush()
    }

  def formatMs(nanosec: Long): String = f"${nanosec.toDouble / 1000000}%.05f"

  def measureScenario(spec: ScenarioSpec): Boolean = {
    var utility = 0
    val measurements = for (i <- 0 until TEST_ROUNDS) yield {
      val (success, time, oneUtility) = solveScenario(spec)
      perf(spec, i, success, time)
      utility = oneUtility
      time
    }

    val measurementsSorted = measurements.sorted
    val min = formatMs(measurements.min)
    val max = formatMs(measurements.max)
    val avg = formatMs(measurements.sum / TEST_ROUNDS)
    val med = formatMs(measurementsSorted(TEST_ROUNDS / 2))
    log(s"Scenario ${spec} solved in avg $avg ms (min: $min, max: $max, med: $med), utility $utility")

    measurements.exists(_ < SOLVER_TIME_LIMIT * TimeUtils.MILLISECONDS_IN_NANOSECONDS)
  }

  def solutionFitsAllWorkers(model: LunchModel): Boolean = {
    val alreadyNotified = model.workers.count(_.notified[RoomAssignedNotification])
    val selectCardinalities = model.problem.instance
      .lunchroomAssignments.selectedMembers
      .map(_.assignees.cardinality.solutionValue)
      .sum
    alreadyNotified + selectCardinalities == model.workers.length
  }

  def solveScenario(spec: ScenarioSpec) = {
    val model = ModelGenerator.modelFromSpec(spec)

    val start = System.nanoTime()
    model.problem.init()
    model.problem.solverLimitTime(SOLVER_TIME_LIMIT)
//    val init = System.nanoTime()
//    while (model.problem.solve() && !solutionFitsAllWorkers(model)) { }
    while (model.problem.solve()) { }
    if (model.problem.exists) {
      model.problem.commit()
//      for (action <- model.problem.actions) println(action)
    }
//    println(model.problem.instance.toStringWithUtility)
    val end = System.nanoTime()
    val time = end - start

    val success = time < SOLVER_TIME_LIMIT * TimeUtils.MILLISECONDS_IN_NANOSECONDS

    (success, time, model.problem.instance.solutionUtility)
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

  def warmup(spec: ScenarioSpec): Unit = {
    var totalTime: Long = 0
    for (_ <- 0 until 100) {
      val (_, time, _) = solveScenario(spec)
      totalTime += time
    }
    log(s"warmup completed in ${formatMs(totalTime)} ms")
  }

  def measure_workerCount_simple(isLunchTime: Boolean): Unit = {
    log(s"===== varying worker count - ${if (isLunchTime) "" else "not "}lunch hour =====")
    val defaultSpec = ScenarioSpec(
      projects = 40,
      lunchrooms = (0, 0),
      workrooms = (100, 50),
      workers = 50,
      hungryWorkers = 0,
      preassignedRooms = 0,
      isLunchTime = isLunchTime,
    )
    warmup(defaultSpec)
    breakable {
      for (workerCount <- 50.to(1000, 50)) {
        val spec = defaultSpec.copy(workers = workerCount)
        if (!measureScenario(spec)) break
      }
    }
  }

  def measure_workerCount_moreProjectsThanRooms: Unit = {
    log("===== varying worker count - more projects than rooms =====")
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
          val spec = defaultSpec.copy(hungryWorkers = workerCount, lunchrooms = (lunchroomCount, 5))
          if (!measureScenario(spec)) break
        }
      }
    }
  }

  def measure_projectsFitRooms: Unit = {
    log("===== assigning worker to rooms when project config matches =====")
    /* technically with number of people available to fit into rooms */
    val defaultSpec = ScenarioSpec(
      projects = 3,
      lunchrooms = (3, 5),
      workrooms = (10, 50),
      workers = 15,
      hungryWorkers = 15,
      preassignedRooms = 0,
      isLunchTime = true,
    )
    warmup(defaultSpec)
    for (workersPerProject <- 5 to 50) {
      val workersCount = workersPerProject * 3
      val spec = defaultSpec.copy(
        lunchrooms = (3, workersPerProject),
        workers = workersCount,
        hungryWorkers = workersCount,
      )
      measureScenario(spec)
    }
  }

  def measure_roomCapacity: Unit = {
    log("===== slowdown with increasing room capacity =====")
    /* technically with number of people available to fit into rooms */
    val defaultSpec = ScenarioSpec(
      projects = 4,
      lunchrooms = (3, 5),
      workrooms = (10, 50),
      workers = 50,
      hungryWorkers = 25,
      preassignedRooms = 0,
      isLunchTime = true,
    )
    warmup(defaultSpec)
    breakable {
      for (lunchroomCapacity <- 5 to 15) {
        val spec = defaultSpec.copy(lunchrooms = (3, lunchroomCapacity))
        if (!measureScenario(spec)) break
      }
    }
  }

  def measure_workerCount_moreRoomsThanProjects: Unit = {
    log("===== varying worker count - more rooms than projects =====")
    val defaultSpec = ScenarioSpec(
      projects = 3,
      lunchrooms = (4, 5),
      workrooms = (10, 50),
      workers = 50,
      hungryWorkers = 0,
      preassignedRooms = 0,
      isLunchTime = true,
    )
    warmup(defaultSpec)
    breakable {
      for (workerCount <- 5 to 40) {
        val spec = defaultSpec.copy(hungryWorkers = workerCount)
        if (!measureScenario(spec)) break
      }
    }
  }

  /*
  def measure_workerCount_randomRoomSizes: Unit = {
    log("===== varying worker count - unevenly sized rooms =====")
    breakable {
      for (workerCount <- 2 to 20) {
        val spec = ScenarioSpec(3, 5, 5, true, workerCount, 0)
        if (!measureScenario(spec)) break
      }
    }
  }

  def measure_roomCount: Unit = {
    log("===== varying room count =====")
    breakable {
      for (roomCount <- 3 to 15) {
        val spec = ScenarioSpec(2, roomCount, 5, false, 20, 0)
        if (!measureScenario(spec)) break
      }
    }
  }

  def measure_preassignedRooms: Unit = {
    log("===== effect of room preassignment =====")
    val MAX_ROOMS = 10
    for (preassignedCount <- MAX_ROOMS.to(0, -1)) {
      breakable {
        for (workersCount <- 5 to 50) {
          val specPreassigned =
            ScenarioSpec(1, MAX_ROOMS, 5, false, workersCount, preassignedCount)
          if (!measureScenario(specPreassigned)) break
        }
      }
    }
  }*/

  def main(args: Array[String]): Unit = {
//    val defaultSpec = ScenarioSpec(
//      projects = 4,
//      lunchrooms = (3, 20),
//      workrooms = (10, 50),
//      workers = 50,
//      hungryWorkers = 50,
//      preassignedRooms = 0,
//      isLunchTime = true,
//    )
//    solveScenario(defaultSpec)
//    return
    warmup
    measure_projectsFitRooms
    return
    measure_workerCount_simple(false)
    measure_workerCount_simple(true)
    measure_workerCount_moreProjectsThanRooms
    measure_workerCount_moreRoomsThanProjects
    measure_roomCapacity

  }
}
