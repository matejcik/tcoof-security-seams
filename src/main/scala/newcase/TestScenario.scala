package newcase

import java.io.{File, PrintWriter}

import newcase.model._

import scala.util.Random
import scala.util.control.Breaks._


object TestScenario {
  val TEST_ROUNDS = 15

  val WORKER_RANGE = 5 to 42
  val ROOM_RANGE = 3 to 15
  val PROJECT_RANGE = 3 to 15
  val CAPACITY_RANGE = 1 to 25

  val logWriter = new PrintWriter(new File("lunch.log"))
  val perfLogWriter = new PrintWriter(new File("lunch-perf.log"))

  def log(s: String): Unit = {
    println(s)
    logWriter.println(s)
    logWriter.flush()
  }

  def formatMs(nanosec: Long): String = f"${nanosec.toDouble / 1000000}%.05f"

  def measureScenario(spec: ScenarioSpec) = {
    var utility = 0
    val measurements = for (i <- 0 until TEST_ROUNDS) yield {
      val (_, perf, oneUtility) = solveScenario(spec)
      perfLogWriter.println(s"${spec.toPerfLine}, $i, $perf")
      perfLogWriter.flush()
      utility = oneUtility
      perf
    }

    val measurementsSorted = measurements.sorted
    val min = formatMs(measurements.min)
    val max = formatMs(measurements.max)
    val avg = formatMs(measurements.sum / TEST_ROUNDS)
    val med = formatMs(measurementsSorted(TEST_ROUNDS / 2))
    log(s"Scenario ${spec} solved in avg $avg ms (min: $min, max: $max, med: $med), utility $utility")
  }

  def solveScenario(spec: ScenarioSpec) = {
    val model = ModelGenerator.modelFromSpec(spec)

    //val fullUtility = model.rooms.map(r => r.capacity * r.capacity).sum

    val start = System.nanoTime()
    model.problem.init()
    val init = System.nanoTime()
    //println(s"   init ran in ${(init - start) / 1000000.0} ms")
    //var foundMaxUtility = false
    while (model.problem.solve()) {
//      if (model.problem.instance.solutionUtility >= fullUtility) {
//        val solutionTime = System.nanoTime()
//        println(s"   found full solution after ${formatMs(solutionTime - init)} ms")
//      }
    }
    if (model.problem.exists) {
      model.problem.commit()
      //      for (action <- model.problem.actions) {
      //        println(action)
      //      }
    }
    val end = System.nanoTime()
//    println(s"   full set took ${formatMs(end - start)} ms")

    (model.problem.exists(), end - start, model.problem.instance.solutionUtility)
  }


  def warmup = {
    val spec = ScenarioSpec(3, 3, 10, false, 10, ProjectAssignment.ROUND_ROBIN)
    var totalTime: Long = 0
    for (_ <- 0 until 100) {
      val (_, time, _) = solveScenario(spec)
      totalTime += time
    }
    log(s"warmup completed in ${formatMs(totalTime)} ms")
  }


  def measure_workerCount: Unit = {
    for (workerCount <- WORKER_RANGE) {
      val specRegular = ScenarioSpec(3, 3, 10, false, workerCount, ProjectAssignment.ROUND_ROBIN)
      measureScenario(specRegular)
      val specRandom = ScenarioSpec(3, 3, 20, true, workerCount, ProjectAssignment.ROUND_ROBIN)
      measureScenario(specRandom)
    }
  }

  def measure_roomCount: Unit = {
    for (roomCount <- ROOM_RANGE) {
      val specRegular = ScenarioSpec(3, roomCount, 10, false, 20, ProjectAssignment.ROUND_ROBIN)
      measureScenario(specRegular)
      val specRandom = ScenarioSpec(3, roomCount, 20, true, 20, ProjectAssignment.ROUND_ROBIN)
      measureScenario(specRandom)
    }
  }

  def measure_projectCount: Unit = {
    for (projectCount <- PROJECT_RANGE) {
      val specRegular = ScenarioSpec(projectCount, 15, 10, false, 30, ProjectAssignment.ROUND_ROBIN)
      measureScenario(specRegular)
      val specRandom = ScenarioSpec(projectCount, 15, 10, false, 30, ProjectAssignment.RANDOM)
      measureScenario(specRandom)
    }
  }


  def main(args: Array[String]): Unit = {

    warmup

    log("===== measuring varying worker counts =====")
    measure_workerCount
    log("===== measuring varying room counts =====")
    measure_roomCount
    log("===== measuring varying project counts =====")
    measure_projectCount
  }
}
