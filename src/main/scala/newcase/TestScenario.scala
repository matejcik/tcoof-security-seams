package newcase

import java.io.{File, PrintWriter}

import newcase.model._
import org.chocosolver.solver.search.loop.lns.neighbors.Neighbor
import org.chocosolver.util.tools.TimeUtils

import scala.util.Random
import scala.util.control.Breaks._


object TestScenario {
  val TEST_ROUNDS = 30
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
      val (success, time, _) = solveScenario(spec)
      perf(spec, i, success, time)
      time
    }

    val measurementsSorted = measurements.sorted
    val min = formatMs(measurements.min)
    val max = formatMs(measurements.max)
    val avg = formatMs(measurements.sum / TEST_ROUNDS)
    val med = formatMs(measurementsSorted(TEST_ROUNDS / 2))
    log(s"Scenario ${spec} solved in avg $avg ms (min: $min, max: $max, med: $med)")

    measurements.exists(_ < SOLVER_TIME_LIMIT * TimeUtils.MILLISECONDS_IN_NANOSECONDS)
  }

  def solutionFitsAllWorkers(model: LunchModel): Boolean = {
    val alreadyNotified = model.workers.count(_.notified[RoomAssignedNotification])
    val selectCardinalities = model.problem.instance.roomAssignments.selectedMembers
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


  def warmup = {
    val spec = ScenarioSpec(3, 3, 10, false, 10, 0)
    var totalTime: Long = 0
    for (_ <- 0 until 100) {
      val (_, time, _) = solveScenario(spec)
      totalTime += time
    }
    log(s"warmup completed in ${formatMs(totalTime)} ms")
  }


  def measure_workerCount_moreProjectsThanRooms: Unit = {
    log("===== varying worker count - more projects than rooms =====")
    breakable {
      for (workerCount <- 2 to 50) {
        val spec = ScenarioSpec(4, 3, 5, false, workerCount, 0)
        if (!measureScenario(spec)) break
      }
    }
  }

  def measure_workerCount_moreRoomsThanProjects: Unit = {
    log("===== varying worker count - more rooms than projects =====")
    breakable {
      for (workerCount <- 2 to 50) {
        val spec = ScenarioSpec(3, 4, 5, false, workerCount, 0)
        if (!measureScenario(spec)) break
      }
    }
  }

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
  }


  def main(args: Array[String]): Unit = {
//    val spec = ScenarioSpec(5, 12, 5, false, 40, 0)
//    solveScenario(spec)

    warmup

    measure_workerCount_moreRoomsThanProjects
    measure_workerCount_moreRoomsThanProjects
    measure_preassignedRooms
    measure_roomCount
  }
}
