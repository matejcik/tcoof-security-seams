package newcase

import java.io.{File, PrintWriter}
import java.time.LocalDateTime

import tcof._

import scala.util.Random
import scala.util.control.Breaks._

class TestScenario extends Model {

  abstract class Room(val room_name: String,
                      val capacity: Int,
                      val targetStatus: WorkerStatus)
      extends Component {
    name(room_name)
  }

  class WorkRoom(name: String, capacity: Int)
      extends Room(name, capacity, WorkerWorking)
  class LunchRoom(name: String, capacity: Int)
      extends Room(name, capacity, WorkerHungry)

  sealed trait WorkerStatus
  case object WorkerWorking extends WorkerStatus
  case object WorkerHungry extends WorkerStatus

  class Worker(name: String, var status: WorkerStatus, var project: Project)
      extends Component {
    name(name)
    var location: Room = null
    var timeSinceLastEaten = 0

    def say(s: String): Unit = println(s"${name} says: ${s}")

    def step: Unit = status match {
      case WorkerWorking => {
        timeSinceLastEaten += 1
        notifications.find(_.isInstanceOf[RoomAssignedNotification]) match {
          case Some(RoomAssignedNotification(room)) => {
            if (location != room)
              say(s"Heading to ${room.name}")
            location = room
          }
          case None => location = null
        }
        if (timeSinceLastEaten > 5 && Random.nextInt(25) < timeSinceLastEaten) {
          say("I'm hungry! Heading out")
          status = WorkerHungry
          clearNotification(RoomAssignedNotification(location))
          location = null
        }
      }
      case WorkerHungry => {
        notifications.find(_.isInstanceOf[RoomAssignedNotification]) match {
          case Some(RoomAssignedNotification(room)) => {
            if (location != room)
              say(s"Heading to ${room.name}")
            location = room
            timeSinceLastEaten = 0
          }
          case None => location = null
        }
        if (Random.nextInt(10) < 2) {
          say("Nom nom! Now back to work")
          status = WorkerWorking
          clearNotification(RoomAssignedNotification(location))
          location = null
        }
      }
    }
  }

  class Project(name: String) extends Component {
    name(name)
  }

  val projects = for (x <- 'A' to 'B')
    yield new Project(s"Project $x")

  val allWorkers = for (i <- 1 to 20) yield {
    val pickProject = projects(Random.nextInt(projects.size))
    new Worker(f"Worker-$i%02d-${pickProject.name}", WorkerWorking, pickProject)
  }

  val lunchrooms = for (i <- 1 to 3)
    yield new LunchRoom(s"Lunchroom-${i}", 5)

  val workrooms = for (i <- 1 to 3)
    yield new WorkRoom(s"Workroom-${i}", 5)

  val rooms = workrooms ++ lunchrooms

  class Everybody extends RootEnsemble {
    name("everybody in the building")

    class RoomAssignment(room: Room) extends Ensemble {
      val project = oneOf(projects)
      val belongHere =
        allWorkers.filter(_.notified(RoomAssignedNotification(room)))
      val notNotified = allWorkers
        .filterNot(_.notified[RoomAssignedNotification])
        .filter(_.status == room.targetStatus)

      val invitees = _addRole(
        "invitees-" + room.name,
        notNotified,
        c => c <= room.capacity - belongHere.size
      )

      constraints {
        project.all(p => invitees.all(_.project == p)) &&
        project.all(p => belongHere.forall(_.project == p))
      }

      utility {
        val occupied = invitees.cardinality + belongHere.size
        occupied * occupied
      }

      notify(invitees.selectedMembers, RoomAssignedNotification(room))
      allow(invitees.selectedMembers, "enter", room)
      allow(belongHere, "enter", room)
    }

    val groups = rules(rooms.map(new RoomAssignment(_)))

    constraints(groups.map(_.invitees).allDisjoint)
  }

  case class RoomAssignedNotification(room: Room) extends Notification

  val everybody = root(new Everybody)
}

object TestScenario {

  def solveScenario(scenario: TestScenario): Unit = {
    scenario.everybody.init()
    while (scenario.everybody.solve()) {
      scenario.everybody._solution.printUtility
    }
    if (scenario.everybody.exists) {
      scenario.everybody.commit()
//      for (action <- scenario.everybody.actions) {
//        println(action)
//      }
    } else {
      println("no solution found :(")
    }
  }

  def main(args: Array[String]): Unit = {
    val scenario = new TestScenario
    for (i <- 0 to 100000) {
      scenario.allWorkers.foreach(_.step)
      solveScenario(scenario)
      println(s":: step $i")
    }
  }
}
