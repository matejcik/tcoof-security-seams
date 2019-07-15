package scenario.lunch

import java.time.LocalTime

import cz.cuni.mff.d3s.trust._

// Different types of rooms
abstract class Room(name: String) extends Component {
  name(s"Room:$name")
}
class LunchRoom(name: String, val capacity: Int)
  extends Room("Lunch" + name)
class WorkRoom(name: String)
  extends Room("Work" + name)

// Worker assigned to a project, can be hungry or not
class Worker(id: Int, val project: Project) extends Component {
  name(s"Worker:$id:${project.name}")
  var hungry = false
  var location: Option[Room] = None

  def isInLunchRoom: Boolean =
    location.map(_.isInstanceOf[LunchRoom]).getOrElse(false)
}

// Project with pre-assigned workrooms
case class Project(name: String, workrooms: Seq[WorkRoom])

// Notification for lunchroom assignment
case class LunchRoomAssigned(room: LunchRoom) extends Notification

class LunchScenario(val projects: Seq[Project],
                    val workers: Seq[Worker],
                    val workrooms: Seq[WorkRoom],
                    val lunchrooms: Seq[LunchRoom]) {

  // Opening times of the building and of the lunchrooms
  val BuildingOpenTime  = LocalTime.of( 7, 30)
  val BuildingCloseTime = LocalTime.of(21,  0)
  val LunchOpenTime     = LocalTime.of(11, 30)
  val LunchCloseTime    = LocalTime.of(15,  0)

  val DefaultNow = LocalTime.of(8, 42)
  var now = DefaultNow

  // mapping projects to lists of workers
  val workersByProject = workers.groupBy(_.project)

  class RoomAssignment extends Ensemble {
    name("assign workers to projects and rooms")

    // list of all hungry workers waiting for a lunchroom
    val hungryWorkers = workers.filter { w =>
      w.hungry &&
      !w.isInLunchRoom &&
      !w.notified[LunchRoomAssigned]
    }

    // Each worker assigned to a project can access all workrooms
    // assigned to that project when the building is open.
    class WorkroomAssignment(project: Project) extends Ensemble {
      name(s"assign workrooms to workers on project ${project.name}")

      situation { (now isAfter BuildingOpenTime) &&
                  (now isBefore BuildingCloseTime) }

      val projectWorkers = workersByProject.getOrElse(project, Seq.empty)
      allow(projectWorkers, "enter", project.workrooms)
    }

    // Each hungry worker will get an assigned lunchroom so that
    // no lunchroom is over capacity and workers from different
    // projects do not meet in the same lunchroom.
    class LunchroomAssignment(room: LunchRoom) extends Ensemble {
      name(s"assign workers to lunchroom ${room.name}")

      // Only activate when lunchrooms are open
      situation { (now isAfter LunchOpenTime) &&
                  (now isBefore LunchCloseTime) }

      // list of previously assigned workers
      val occupants = workers.filter { w =>
        w.notified(LunchRoomAssigned(room)) ||
        w.location.contains(room)
      }

      // newly-assigned hungry workers must fit into free space
      val freeSpaces = room.capacity - occupants.size
      val assignees = subsetOf(hungryWorkers, _ <= freeSpaces)

      val eaters = unionOf(occupants, assignees)
      constraint { eaters.allEqual(_.project) }

      // Set the solution utility to square of the number of occupants,
      // i.e., prefer many workers in one room over few workers in many rooms
      utility {
        val occupied = assignees.cardinality + occupants.size
        occupied * occupied
      }

      // grant access rights and notify newly selected hungry workers
      notify(assignees, LunchRoomAssigned(room))
      allow(eaters, "enter", room)
    }

    val workroomAssignments =
      rules(projects.map(new WorkroomAssignment(_)))
    val lunchroomAssignments =
      rules(lunchrooms.map(new LunchroomAssignment(_)))

    // ensure that a worker is not assigned to more than one lunchroom
    constraint(lunchroomAssignments.map(_.assignees).allDisjoint)
  }

  val policy = Policy.root(new RoomAssignment)
}
