package scenario

import cz.cuni.mff.d3s.enact.Policy
import org.chocosolver.util.tools.TimeUtils
import scenario.lunch._
import scenario.lunch.LunchSpec._
import scenario.testing.{Spec, TestHarness}

import scala.collection.mutable
import scala.util.Random

class LunchSimulator {
  val DefaultSpec = LunchSpec(
    projects = 10,
    lunchrooms = (5, 40),
    workrooms = (30, 50),
    workers = 500,
    hungryWorkers = 0,
    fillRooms = 0,
    isLunchTime = true,
  )

  val scenario = DefaultSpec.makeScenario()
  val random = new Random()

  var step: Int = 0
  val workersNotifiedAt = mutable.HashMap.empty[Worker, Int]
  val workersStartedEating = mutable.HashMap.empty[Worker, Int]
  val workersDone = mutable.HashSet.empty[Worker]

  final val StepsToLunchRoom = 5
  final val MinEatingTime = 5
  final val MaxEatingTime = 20

  final val HungerProbability = 0.005

  def simulationStep(): Unit = {
    step += 1

    // workers become hungry
    var hungry = 0
    for (w <- scenario.workers.filterNot(_.hungry).filterNot(workersDone contains _)) {
      if (random.nextFloat() <= HungerProbability) {
        w.hungry = true
        hungry += 1
      }
    }

    // workers finish eating
    var finished = 0
    for ((w, time) <- workersStartedEating) {
      val timeEating = step - time
      val adaptedTime = timeEating - MinEatingTime
      val probFrac = adaptedTime / (MaxEatingTime - MinEatingTime)
      val prob = math.pow(probFrac, 2)
      if (random.nextFloat() <= prob) {
        workersStartedEating -= w
        w.hungry = false
        w.location = None
        workersDone += w
        finished += 1
      }
    }

    // workers arrive at lunch
    var arrived = 0
    for ((w, time) <- workersNotifiedAt) {
      val timeEnRoute = step - time
      val prob = math.pow(timeEnRoute / StepsToLunchRoom, 3)
      if (random.nextFloat() <= prob) {
        workersNotifiedAt -= w
        workersStartedEating += w -> step
        val lunchroom = w.notifications.collect { case LunchRoomAssigned(l) => l }.head
        w.location = Some(lunchroom)
        w.clearNotification(LunchRoomAssigned(lunchroom))
        arrived += 1
      }
    }

    // notified workers head out
    var seated = 0
    for (w <- scenario.workers.filter(_.notified[LunchRoomAssigned]).filterNot(_.isInLunchRoom)) {
      if (!workersNotifiedAt.contains(w)) {
        workersNotifiedAt += w -> step
        seated += 1
      }
    }

    val starving = scenario.workers.filter(_.hungry).filterNot(_.isInLunchRoom).size

//    println(
//      s"step $step: starving $starving, becameHungry $hungry, headedOut $seated, arrived $arrived, finished $finished"
//    )
//    val lunchroomUtilization = for (l <- scenario.lunchrooms) yield {
//      val used = scenario.workers.count(_.location == Some(l))
//      s"${l.name}: $used/${l.capacity}"
//    }
//    println(lunchroomUtilization.mkString(" :: "))
  }

  def allFinished: Boolean = workersDone.size == scenario.workers.size

}

case class SimulatorSpec(simulator: LunchSimulator, iter: Int) extends Spec[LunchScenario] {
  override def makeScenario(): LunchScenario = {
    simulator.simulationStep()
    simulator.scenario
  }

  override def policy(scenario: LunchScenario): Policy[_] = scenario.policy
  override def toPerfLine: String = iter.toString
}

object RandomLunch extends TestHarness[LunchSimulator] {
  override type ScenarioSpec = SimulatorSpec

  override def solveScenario(spec: ScenarioSpec): Measure = {
    val scenario = spec.makeScenario()
    val policy = scenario.policy

    val start = System.nanoTime()
    policy.init()
    policy.solverLimitTime(SOLVER_TIME_LIMIT)
    while (policy.solve()) {}
    if (policy.exists) policy.commit()
    val end = System.nanoTime()
    val time = end - start

    Measure(true, time, 0)
  }

  override val TEST_ROUNDS: Int = 1500

  def main(args: Array[String]): Unit = {
    val warmupSim = new LunchSimulator
    val warmupSpec = SimulatorSpec(warmupSim, 0)
    warmup(warmupSpec)
    measure(
      "simulated",
      "simulating lunchtime in a big company",
    ) { m =>
      for (i <- 1 to 100) {
        val sim = new LunchSimulator
        m(SimulatorSpec(sim, i))
      }

    }
  }
}
