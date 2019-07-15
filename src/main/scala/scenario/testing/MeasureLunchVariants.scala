package scenario.testing

import scenario.lunch.LunchSpec._
import scenario.lunch._

import scala.util.Random
import scala.util.control.Breaks._

object MeasureLunchVariants extends TestHarness[LunchScenario] {
  override type ScenarioSpec = LunchSpec

  def solveOneByOne(spec: ScenarioSpec): Measure = {
    val model = spec.makeScenario()

    val hungryWorkers = Random.shuffle(model.workers.filter(_.hungry))
    hungryWorkers.foreach(_.hungry = false)

    val start = System.nanoTime()

    breakable {
      for (worker <- hungryWorkers) {
        worker.hungry = true

        model.policy.resolve(SOLVER_TIME_LIMIT)

        val currentTime = System.nanoTime() - start
        if (currentTime > LIMIT_NANO) break
      }
    }
    //    println(model.problem.instance.toStringWithUtility)
    val end = System.nanoTime()
    val time = end - start

    val success = model.policy.exists && time < LIMIT_NANO
    val utility = if (success) model.policy.solutionUtility else 0
    Measure(success, time, utility)
  }

  def warmup: Unit = {
    val spec = LunchSpec(
      projects = 3,
      lunchrooms = (3, 10),
      workrooms = (3, 10),
      workers = 10,
      hungryWorkers = 10,
      fillRooms = 0,
      isLunchTime = true,
    )
    warmup(spec)
  }

  def measure_moreProjectsThanRooms =
    measure(
      "moreprojects",
      "more projects than rooms - iterate project and worker count",
      solverFunc = solveScenario
    ) { m =>
      val defaultSpec = LunchSpec(
        projects = 7,
        lunchrooms = (3, 5),
        workrooms = (10, 50),
        workers = 50,
        hungryWorkers = 5,
        fillRooms = 0,
        isLunchTime = true,
      )
      warmup(defaultSpec)
      for (projectCount <- 2 to 10) {
        breakable {
          for (workerCount <- 5 to 50) {
            val spec = defaultSpec.copy(
              hungryWorkers = workerCount,
              projects = projectCount,
            )
            m(spec)
          }
        }
      }
    }

  def measure_moreRoomsThanProjects =
    measure(
      "morerooms-optimizing",
      "more rooms than projects - optimizing solver",
      solverFunc = solveScenario
    ) { m =>
      val defaultSpec = LunchSpec(
        projects = 3,
        lunchrooms = (4, 10),
        workrooms = (10, 50),
        workers = 50,
        hungryWorkers = 5,
        fillRooms = 0,
        isLunchTime = true,
      )
      warmup(defaultSpec)
      for (workerCount <- 5 to 40) {
        val spec = defaultSpec.copy(hungryWorkers = workerCount)
        if (!m(spec)) break
      }
    }

  def measure_oneByOne_growingParams =
    measure(
      "oneworker-params",
      "assigning one worker with growing parameters",
    ) { m =>
      val defaultSpec = LunchSpec(
        projects = 20,
        lunchrooms = (20, 100),
        workrooms = (10, 50),
        workers = 5000,
        hungryWorkers = 1,
        fillRooms = 0,
        isLunchTime = true,
      )
      warmup(defaultSpec)

      //for (projectCount <- Seq(5, 10, 20)) {
      breakable {
        for (lunchCount <- 50.to(500, 25)) {
          val spec = defaultSpec.copy(
            lunchrooms = (lunchCount, 100),
            //projects = projectCount,
          )
          if (!m(spec)) break
        }
      }
    //}
    }

  def measure_blabla =
    measure("blabla", "measuring some things") { m =>
      val defaultSpec = LunchSpec(
        projects = 7,
        lunchrooms = (3, 5),
        workrooms = (10, 50),
        workers = 500,
        hungryWorkers = 5,
        fillRooms = 0,
        isLunchTime = true,
      )
      warmup(defaultSpec)
      for (projectCount <- 3 to 7) {
        for (workerCount <- 5 to 40) {
          val emptySpec = defaultSpec.copy(
            projects = projectCount,
            hungryWorkers = workerCount,
          )
          m(emptySpec)

          val oneSpec = emptySpec.copy(
            fillRooms = 1,
          )
          m(oneSpec)

          val fullSpec = emptySpec.copy(
            fillRooms = 5,
          )
          m(fullSpec)
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

    measure_moreProjectsThanRooms
    measure_oneByOne_growingParams
    measure_moreRoomsThanProjects
  }
}