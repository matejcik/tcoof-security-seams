package scenario

import scenario.model._
import scenario.model.ScenarioSpec._
import scenario.testing.TestHarness

import scala.util.Random
import scala.util.control.Breaks._

object TestScenario extends TestHarness[LunchScenario] {

  def solutionFitsAllWorkers(model: LunchScenario): Boolean = {
    val hungryWorkers = model.workers.filter(_.hungry)
    val alreadyNotified =
      hungryWorkers.count(_.notified[LunchRoomAssigned])
    val selectCardinalities =
      model.policy.instance.lunchroomAssignments.selectedMembers
        .map(_.assignees.cardinality.asInt)
        .sum
    alreadyNotified + selectCardinalities == hungryWorkers.length
  }

  def solveUntilFitsAll(spec: ScenarioSpec): Measure = {
    val model = spec.makeScenario()

    val start = System.nanoTime()
    model.policy.init()
    model.policy.solverLimitTime(SOLVER_TIME_LIMIT)
    //    val init = System.nanoTime()
    while (model.policy.solve() && !solutionFitsAllWorkers(model)) {}
    val end = System.nanoTime()
    val time = end - start

    val success = time < LIMIT_NANO

    Measure(success, time, model.policy.solutionUtility)
  }

  def solveOneByOne(spec: ScenarioSpec): Measure = {
    val model = spec.makeScenario()

    val hungryWorkers = Random.shuffle(model.workers.filter(_.hungry))
    hungryWorkers.foreach(_.hungry = false)

    val start = System.nanoTime()

    breakable {
      for (worker <- hungryWorkers) {
        worker.hungry = true

        model.policy.init()
        model.policy.solverLimitTime(SOLVER_TIME_LIMIT)
        while (model.policy.solve()) {}
        if (model.policy.exists) model.policy.commit()

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

  def measure_oneByOne_growingParams =
    measure(
      "oneworker-params",
      "assigning one worker with growing parameters",
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
      warmup(defaultSpec)

      for (projectCount <- Seq(5, 10, 20)) {
        breakable {
          for (lunchCount <- 10.to(150, 10)) {
            val spec = defaultSpec.copy(
              lunchrooms = (lunchCount, 100),
              projects = projectCount,
            )
            if(!m(spec)) break
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

    measure_workerCount_simple
    measure_moreRoomsThanProjects_compareMethods
    measure_oneByOne_growingParams

    measure_moreProjectsThanRooms_compareMethods
  }
}
