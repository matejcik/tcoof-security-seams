package scenario

import scenario.model._
import scenario.model.ScenarioSpec._
import scenario.testing.TestHarness

import scala.collection.mutable
import scala.util.Random

object RandomLunch extends TestHarness[LunchScenario] {

  val DefaultSpec = ScenarioSpec(
    projects = 10,
    lunchrooms = (5, 20),
    workrooms = (1, 1),
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

  final val StepsToLunchRoom = 10
  final val MinEatingTime = 5
  final val MaxEatingTime = 20

  final val HungerProbability = 0.002

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

    println(s"step $step: starving $starving, becameHungry $hungry, headedOut $seated, arrived $arrived, finished $finished")
    val lunchroomUtilization = for (l <- scenario.lunchrooms) yield {
      val used = scenario.workers.filter(_.location == Some(l)).size
      s"${l.name}: $used/${l.capacity}"
    }
    println(lunchroomUtilization.mkString(" :: "))
  }

  def allFinished: Boolean = workersDone.size == scenario.workers.size

  def solveScenario(): Measure = {
    simulationStep()
    val policy = scenario.policy

    val start = System.nanoTime()
    policy.init()
    policy.solverLimitTime(SOLVER_TIME_LIMIT)
    while (policy.solve()) {}
    if (policy.exists) policy.commit()
    val end = System.nanoTime()
    val time = end - start

    val success = policy.exists && time < LIMIT_NANO
    val utility = if (policy.exists) policy.solutionUtility else -1

    println(s"solved in ${formatMs(time)} ms, utility $utility")
    Measure(success, time, utility)
  }

  def main(args: Array[String]): Unit = {
    val start = System.nanoTime()
    while (!allFinished) {
      simulationStep()
      solveScenario()
    }
    val end = System.nanoTime()
    val time = end - start
    println(s"all finished at step $step, after ${formatMs(time)} ms")
  }
}
