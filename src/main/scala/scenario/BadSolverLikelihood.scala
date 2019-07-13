package scenario

import scenario.model._
import scenario.model.ScenarioSpec._
import scenario.testing.TestHarness

object BadSolverLikelihood extends TestHarness[LunchScenario] {

  def measure_growingProjects =
    measure(
      "badsolver-growingprojects",
      "increase number of projects, keep solution simple",
    ) { m =>
      val defaultSpec = ScenarioSpec(
        projects = 1,
        lunchrooms = (1, 4),
        workrooms = (10, 50),
        workers = 5000,
        hungryWorkers = 200,
        fillRooms = 0,
        isLunchTime = true,
      )
      warmup(defaultSpec)
      for (projects <- 1 to 30) {
        val spec = defaultSpec.copy(
          projects = projects,
          lunchrooms = (projects, 5),
          hungryWorkers = projects * 5,
        )
        m(spec)
      }
    }

  override val TEST_ROUNDS: Int = 500
  override val SOLVER_TIME_LIMIT: Long = 3L * 1000

  def main(args: Array[String]): Unit = {
    measure_growingProjects
  }
}
