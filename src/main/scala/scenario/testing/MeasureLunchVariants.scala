package scenario.testing

import scenario.lunch.LunchSpec._
import scenario.lunch._

import scala.util.Random
import scala.util.control.Breaks._

/** Measure performance on synthetic problems. */
object MeasureLunchVariants extends TestHarness[LunchScenario] {
  override type ScenarioSpec = LunchSpec

  /** Solve scenario by submitting hungry workers one by one.
    *
    * Used in old test cases.
    */
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

  /** Measure scenario with more projects than rooms.
    *
    * @see thesis section 7.3.2
    */
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

  /** Measure scenario with more rooms than projects.
    *
    * @see thesis section 7.3.2
    */
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

  /** Measure performance of seating one worker
    *
    * @see thesis section 7.3.5
    */
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

  def main(args: Array[String]): Unit = {
    measure_moreProjectsThanRooms
    measure_oneByOne_growingParams
    measure_moreRoomsThanProjects
  }
}
