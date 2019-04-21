package k4case

import java.io.{File, PrintWriter}
import java.time.LocalDateTime

import tcof._
import scala.util.control.Breaks._

case class TestScenarioSpec(
                             workersOnTimePerWorkplaceCount: Int,
                             workersLatePerWorkplaceCount: Int,
                             workersOnStandbyCount: Int,
                             factoriesCount: Int,
                             startTs: String,
                             measureTs: String
                           )

case class Position(x: Double, y: Double)


class TestScenario(scenarioParams: TestScenarioSpec) extends Model with ModelGenerator {

  val startTimestamp = LocalDateTime.parse("2018-12-03T08:00:00")
  var now = startTimestamp

  case class WorkerPotentiallyLateNotification(shift: Shift, worker: Worker) extends Notification

  case class AssignmentCanceledNotification(shift: Shift) extends Notification
  case class StandbyNotification(shift: Shift) extends Notification
  case class WorkerReplacedNotification(shift: Shift, worker: Worker) extends Notification

  case class ScenarioEvent(timestamp: LocalDateTime, eventType: String, person: String, position: Position)

  class Door(
              val id: String,
              val position: Position
            ) extends Component {
    name(s"Door ${id}")

    override def toString = s"Door($id, $position)"
  }

  class Dispenser(
                   val id: String,
                   val position: Position
                 ) extends Component {
    name(s"Protection equipment dispenser ${id}")

    override def toString = s"Dispenser($id, $position)"
  }

  class Worker(
                val id: String,
                var position: Position,
                val capabilities: Set[String],
                var hasHeadGear: Boolean
              ) extends Component {
    name(s"Worker ${id}")

    override def toString = s"Worker($id, $position, $capabilities)"

    def isAt(room: Room) = room.positions.contains(position)
  }

  abstract class Room(
              val id: String,
              val positions: List[Position],
              val entryDoor: Door
            ) extends Component {
    name(s"Room ${id}")
  }

  class WorkPlace(
                   id: String,
                   positions: List[Position],
                   entryDoor: Door
                 ) extends Room(id, positions, entryDoor) {
    name(s"WorkPlace ${id}")

    var factory: Factory = _

    override def toString = s"WorkPlace($id, $positions, $entryDoor)"
  }

  class Factory(
                 id: String,
                 positions: List[Position],
                 entryDoor: Door,
                 val dispenser: Dispenser,
                 val workPlaces: List[WorkPlace]
               ) extends Room(id, positions, entryDoor) {
    name(s"Factory ${id}")

    for (workPlace <- workPlaces) {
      workPlace.factory = this
    }

    override def toString = s"Factory($id, $positions, $entryDoor, $dispenser, $workPlaces)"
  }

  class Shift(
               val id: String,
               val startTime: LocalDateTime,
               val endTime: LocalDateTime,
               val workPlace: WorkPlace,
               val foreman: Worker,
               val workers: List[Worker],
               val standbys: List[Worker],
               val assignments: Map[Worker, String]
             ) extends Component {
    name(s"Shift ${id}")

    override def toString = s"Shift($startTime, $endTime, $workPlace, $foreman, $workers, $standbys, $assignments)"
  }


  val factoryIds = (1 to scenarioParams.factoriesCount).map(idx => f"factory$idx%02d")

  import ModelDSL._
  val (workersMap, factoriesMap, shiftsMap) = withModel { implicit builder =>
    for ((factoryId, factoryIdx) <- factoryIds.zipWithIndex) {
      withFactory(factoryId, 0, 0) { implicit scope =>
        for (wp <- List("A", "B", "C")) {
          val foremanId = s"$factoryId-$wp-foreman"
          withWorker(foremanId, Set("A", "B", "C", "D", "E"))

          val workersOnTime = (1 to scenarioParams.workersOnTimePerWorkplaceCount).map(idx => f"$factoryId%s-$wp%s-ontime-$idx%03d")
          for (id <- workersOnTime) {
            withWorker(id, Set("A", "B", "C", "D", "E"))
          }

          val workersLate = (1 to scenarioParams.workersLatePerWorkplaceCount).map(idx => f"$factoryId%s-$wp%s-late-$idx%03d")
          for (id <- workersLate) {
            withWorker(id, Set("A", "B", "C", "D", "E"))
          }

          val workersOnStandby = (1 to scenarioParams.workersOnStandbyCount).map(idx => f"$factoryId%s-standby-$idx%03d")

          for (id <- workersOnStandby) {
            withWorker(id, Set("A", "B", "C", "D", "E"))
          }

          val workersInShift = workersOnTime ++ workersLate

          withShift(
            wp,
            startTimestamp plusHours 1,
            startTimestamp plusHours 9,
            wp,
            foremanId,
            workersInShift.toList,
            workersOnStandby.toList,
            workersInShift.map(wrk => (wrk, "A")).toMap
          )
        }
      }
    }
  }

  import EventsDSL._
  val events = withEvents { implicit builder =>
    for (factoryId <- factoryIds) {
      // foremen
      withWorkerInShiftA(s"$factoryId-A-foreman", startTimestamp)
      withWorkerInShiftB(s"$factoryId-B-foreman", startTimestamp)
      withWorkerInShiftC(s"$factoryId-C-foreman", startTimestamp)

      // Workers that are on time
      for (idx <- 1 to scenarioParams.workersOnTimePerWorkplaceCount) {
        withWorkerInShiftA(f"$factoryId%s-A-ontime-$idx%03d", startTimestamp)
        withWorkerInShiftB(f"$factoryId%s-B-ontime-$idx%03d", startTimestamp)
        withWorkerInShiftC(f"$factoryId%s-C-ontime-$idx%03d", startTimestamp)
      }

      // Worker that is late - no events
    }
  }


  class FactoryTeam(factory: Factory) extends RootEnsemble {
    name(s"Factory team ${factory.id}")

    class ShiftTeam(shift: Shift) extends Ensemble {
      name(s"Shift team ${shift.id}")

      // These are like invariants at a given point of time
      val calledInStandbys = shift.standbys.filter(wrk => wrk notified StandbyNotification(shift))
      val availableStandbys = shift.standbys diff calledInStandbys

      val canceledWorkers = shift.workers.filter(wrk => wrk notified AssignmentCanceledNotification(shift))
      val canceledWorkersWithoutStandby = canceledWorkers.filterNot(wrk => shift.foreman notified WorkerReplacedNotification(shift, wrk))

      val assignedWorkers = (shift.workers union calledInStandbys) diff canceledWorkers


      object AccessToFactory extends Ensemble { // Kdyz se constraints vyhodnoti na LogicalBoolean, tak ten ensemble vubec nezatahujeme solver modelu a poznamename si, jestli vysel nebo ne
        name(s"AccessToFactory")

        situation {
          (now isAfter (shift.startTime minusMinutes 30)) &&
            (now isBefore (shift.endTime plusMinutes 30))
        }

        allow(shift.foreman, "enter", shift.workPlace.factory)
        allow(assignedWorkers, "enter", shift.workPlace.factory)
      }

      object AccessToDispenser extends Ensemble {
        name(s"AccessToDispenser")

        situation {
          (now isAfter (shift.startTime minusMinutes 15)) &&
            (now isBefore shift.endTime)
        }

        allow(assignedWorkers, "use", shift.workPlace.factory.dispenser)
      }

      object AccessToWorkplace extends Ensemble { // Kdyz se constraints vyhodnoti na LogicalBoolean, tak ten ensemble vubec nezatahujeme solver modelu a poznamename si, jestli vysel nebo ne
        name(s"AccessToWorkplace")

        val workersWithHeadGear = (shift.foreman :: assignedWorkers).filter(wrk => wrk.hasHeadGear)

        situation {
          (now isAfter (shift.startTime minusMinutes 30)) &&
            (now isBefore (shift.endTime plusMinutes 30))
        }

        allow(workersWithHeadGear, "enter", shift.workPlace)
      }



      object NotificationAboutWorkersThatArePotentiallyLate extends Ensemble {
        name(s"NotificationAboutWorkersThatArePotentiallyLate")

        val workersThatAreLate = assignedWorkers.filter(wrk => !(wrk isAt shift.workPlace.factory))

        situation {
          now isAfter (shift.startTime minusMinutes 20)
        }

        workersThatAreLate.foreach(wrk => notify(shift.foreman, WorkerPotentiallyLateNotification(shift, wrk)))

        allow(shift.foreman, "read.personalData.phoneNo", workersThatAreLate)
        allow(shift.foreman, "read.distanceToWorkPlace", workersThatAreLate)
      }


      object CancellationOfWorkersThatAreLate extends Ensemble {
        name(s"CancellationOfWorkersThatAreLate")

        val workersThatAreLate = assignedWorkers.filter(wrk => !(wrk isAt shift.workPlace.factory))

        situation {
          now isAfter (shift.startTime minusMinutes 15)
        }

        notify(workersThatAreLate, AssignmentCanceledNotification(shift))
      }

      object AssignmentOfStandbys extends Ensemble {
        name(s"AssignmentOfStandbys")

        class StandbyAssignment(canceledWorker: Worker) extends Ensemble {
          name(s"StandbyAssignment for ${canceledWorker.id}")

          val standby = oneOf(availableStandbys)

          constraints {
            standby.all(_.capabilities contains shift.assignments(canceledWorker))
          }
        }

        val standbyAssignments = rules(canceledWorkersWithoutStandby.map(new StandbyAssignment(_)))

        val selectedStandbys = unionOf(standbyAssignments.map(_.standby))

        situation {
          (now isAfter (shift.startTime minusMinutes 15)) &&
          (now isBefore shift.endTime)
        }

        constraints {
          standbyAssignments.map(_.standby).allDisjoint
        }

        notify(selectedStandbys.selectedMembers, StandbyNotification(shift))
        canceledWorkersWithoutStandby.foreach(wrk => notify(shift.foreman, WorkerReplacedNotification(shift, wrk)))
      }

      object NoAccessToPersonalDataExceptForLateWorkers extends Ensemble {
          name(s"NoAccessToPersonalDataExceptForLateWorkers")

          val workersPotentiallyLate =
              if ((now isAfter (shift.startTime minusMinutes 20)) && (now isBefore shift.startTime))
                  assignedWorkers.filter(wrk => !(wrk isAt shift.workPlace.factory))
              else
                  Nil

          val workers = shift.workers diff workersPotentiallyLate

          deny(shift.foreman, "read.personalData", workers, PrivacyLevel.ANY)
          deny(shift.foreman, "read.personalData", workersPotentiallyLate, PrivacyLevel.SENSITIVE)
      }

      rules(
        // Grants
        AccessToFactory,
        AccessToDispenser,
        AccessToWorkplace,
        NotificationAboutWorkersThatArePotentiallyLate,
        CancellationOfWorkersThatAreLate,
        AssignmentOfStandbys,

        // Assertions
        NoAccessToPersonalDataExceptForLateWorkers
      )
    }

    val shiftTeams = rules(shiftsMap.values.filter(shift => shift.workPlace.factory == factory).map(shift => new ShiftTeam(shift)))

    constraints {
      shiftTeams.map(_.AssignmentOfStandbys.selectedStandbys).allDisjoint
    }
  }

  val factoryTeams = factoriesMap.values.map(factory => root(new FactoryTeam(factory)))
}



object TestScenario {
  println("Saving log to test-scenario.log")
  val logPrintWriter = new PrintWriter(new File("test-scenario.log"))
  val perfLogPrintWriter = new PrintWriter(new File("test-scenario-perf.log"))

  def log(): Unit = {
    logPrintWriter.println()
    logPrintWriter.flush()
  }

  def log(msg: Any): Unit = {
    logPrintWriter.println(msg)
    logPrintWriter.flush()
  }

  def logPerf(measurePhase: Int, factoriesCount: Int, workersPerWorkplaceCount: Int, workersLateRatio: Double, iterationNo: Int, time: Long): Unit = {
    perfLogPrintWriter.println(s"${measurePhase}, ${factoriesCount}, ${workersPerWorkplaceCount}, ${workersLateRatio}, ${iterationNo}, ${time}")
    perfLogPrintWriter.flush()
  }


  def createScenarioSpec(factoriesCount: Int, workersPerWorkplaceCount: Int, workersLateRatio: Double, measurePhase: Int) = {
    val workersLateCount = Math.round(workersPerWorkplaceCount * workersLateRatio).toInt

    TestScenarioSpec(
      workersOnTimePerWorkplaceCount = workersPerWorkplaceCount - workersLateCount,
      workersLatePerWorkplaceCount = workersLateCount,
      workersOnStandbyCount = workersLateCount * 5,
      factoriesCount = factoriesCount,
      startTs = "2018-12-03T08:00:00",
      measureTs = if (measurePhase == 0) "2018-12-03T08:43:00" else "2018-12-03T08:47:00"
    )
  }

  def main(args: Array[String]): Unit = {
    val warmupCount = 10
    val measurementsCount = 100
    val solverLimitTime = 60000000000L

    for (measurePhase <- List(0, 1)) {
      for (factoriesCount <- List(1)) {
        for (workersLateRatio <- List(0.05, 0.1, 0.15, 0.2)) {
          breakable {
            for (workersPerWorkplaceCount <- 50.to(500, 50)) {
              val scenarioSpec = createScenarioSpec(factoriesCount, workersPerWorkplaceCount, workersLateRatio, measurePhase)

              val scenario = new TestScenario(scenarioSpec)

              val factoryTeams = scenario.factoryTeams

              log()
              log()
              log(s"====================================================================================================================================")
              log(s"Executing scenario ${scenarioSpec}")
              log(s"====================================================================================================================================")

              val startTs = LocalDateTime.parse(scenarioSpec.startTs)
              val measurementsTs = LocalDateTime.parse(scenarioSpec.measureTs)

              var currentTs = startTs
              var prevTs = startTs minusMinutes 1

              while (currentTs isBefore measurementsTs) {
                scenario.now = currentTs

                //val events = scenario.events filter(_.timestamp == scenario.now)
                val events = scenario.events filter (ev => (ev.timestamp isAfter prevTs) && !(ev.timestamp isAfter currentTs))

                // log()
                log("Time: " + currentTs)
                // log("Events: " + events)

                for (event <- events) {
                  val worker = scenario.workersMap(event.person)
                  worker.position = event.position

                  if (event.eventType == "access-dispenser") {
                    worker.hasHeadGear = true
                  }
                }


                val perfStartTime = System.currentTimeMillis()
                var duration, perfEndTime = 0L

                for (factoryTeam <- factoryTeams) {
                  factoryTeam.init()
                  factoryTeam.solverLimitTime(solverLimitTime)
                  factoryTeam.solve()

                  perfEndTime = System.currentTimeMillis()
                  duration = perfEndTime - perfStartTime

                  if (factoryTeam.exists && duration <= solverLimitTime) {
                    // log("Utility: " + shiftTeams.instance.solutionUtility)
                    // log(shiftTeams.instance.toString)

                    factoryTeam.commit()

                    // for (action <- shiftTeams.actions) {
                    //  log(action)
                    // }

                  } else {

                    log("Error. No solution exists.")
                    break()
                  }
                }

                prevTs = currentTs
                currentTs = currentTs plusMinutes 1
              }

              scenario.now = measurementsTs
              log()
              log(s"Warmup (time: ${scenario.now})")

              for (measurementIdx <- 0 until warmupCount) {
                val perfStartTime = System.nanoTime()
                var duration, perfEndTime = 0L

                for (factoryTeam <- factoryTeams) {

                  factoryTeam.init()
                  factoryTeam.solverLimitTime(solverLimitTime)
                  factoryTeam.solve()

                  perfEndTime = System.nanoTime()
                  duration = perfEndTime - perfStartTime

                  if (!factoryTeam.exists || duration > solverLimitTime) {
                    log("Error. No solution exists.")
                    break()
                  }
                }

                log(f"${measurementIdx}%04d - ${duration / 1000000.0}%f milliseconds")
              }


              log()
              log(s"Measurements (time: ${scenario.now})")

              for (measurementIdx <- 0 until measurementsCount) {
                val perfStartTime = System.nanoTime()
                var duration, perfEndTime = 0L

                for (factoryTeam <- factoryTeams) {
                  factoryTeam.init()
                  factoryTeam.solverLimitTime(solverLimitTime)
                  factoryTeam.solve()

                  perfEndTime = System.nanoTime()
                  duration = perfEndTime - perfStartTime

                  if (!factoryTeam.exists || duration > solverLimitTime) {
                    log("Error. No solution exists.")
                    break()
                  }
                }

                log(f"${measurementIdx}%04d - ${duration / 1000000.0}%f milliseconds")
                logPerf(measurePhase, factoriesCount, workersPerWorkplaceCount, workersLateRatio, measurementIdx, perfEndTime - perfStartTime)
              }
            }
          }
        }
      }
    }
  }
}
