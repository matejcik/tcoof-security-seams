package scenario.model

import java.time.{LocalDate, LocalDateTime, LocalTime}

import tcof._

// Different types of rooms
abstract class Room(name: String, val capacity: Int) extends Component {
  name(s"Room:$name")
}

class LunchRoom(name: String, capacity: Int) extends Room("Lunch" + name, capacity)
class WorkRoom(name: String, capacity: Int) extends Room("Work" + name, capacity)

// Project with pre-assigned workrooms
class Project(name: String, val workrooms: Seq[WorkRoom]) extends Component {
  name(s"Project:$name")
}

// Worker assigned to a project, can be hungry or not
class Worker(id: Int, val project: Project) extends Component {
  name(s"Worker:$id:${project.name}")
  var hungry = false
}

// Notification for lunchroom assignment
case class RoomAssignedNotification(room: Room) extends Notification

class LunchModel(val projects: Seq[Project],
                 val workers: Seq[Worker],
                 val workrooms: Seq[WorkRoom],
                 val lunchrooms: Seq[LunchRoom],
) extends Model {

  // Opening times of the building and of the lunchrooms
  val BUILDING_OPEN_TIME = LocalTime.of(7, 30)
  val BUILDING_CLOSE_TIME = LocalTime.of(21, 0)
  val LUNCH_OPEN_TIME = LocalTime.of(11, 30)
  val LUNCH_CLOSE_TIME = LocalTime.of(15, 0)

  val DEFAULT_NOW = LocalTime.of(8, 42)
  var now = DEFAULT_NOW

  class RoomAssignment extends RootEnsemble {
    name("assign workers to projects and rooms")

    // mapping projects to lists of workers
    private val _workersGroupByProject = workers.groupBy(_.project)
    val workersByProject =
      projects.map(p => p -> _workersGroupByProject.getOrElse(p, Seq.empty)).toMap

    // list of all hungry workers waiting for a lunchroom
    val hungryWorkers =
      workers.filter(_.hungry).filterNot(_.notified[RoomAssignedNotification])

    // Each worker assigned to a project can access all workrooms
    // assigned to that project when the building is open.
    class WorkroomAssignment(project: Project) extends Ensemble {
      name(s"allow workers on project ${project.name} to enter assigned rooms")

      situation {
        (now isAfter BUILDING_OPEN_TIME) && (now isBefore BUILDING_CLOSE_TIME)
      }

      allow(workersByProject(project), "enter", project.workrooms)
    }

    // Each hungry worker will get an assigned lunchroom so that
    // no lunchroom is over capacity and workers from different
    // projects do not meet in the same lunchroom.
    class LunchroomAssignment(room: LunchRoom) extends Ensemble {
      name(s"assign workers to room ${room.name}")

      // Only activate when lunchrooms are open
      situation {
        (now isAfter LUNCH_OPEN_TIME) && (now isBefore LUNCH_CLOSE_TIME)
      }

      // list of previously assigned workers
      val occupants = workers.filter(_.notified(RoomAssignedNotification(room)))

      // newly-assigned hungry workers must fit into free space
      val freeSpaces = room.capacity - occupants.size
      val assignees = subsetOf(hungryWorkers, _ <= freeSpaces)

      val project = oneOf(projects)

      // all selected workers must belong to the selected project
      constraints {
        project.all(p => assignees.all(_.project == p)) &&
          project.all(p => occupants.forall(_.project == p))
      }

      // Set the solution utility to square of the number of occupants,
      // i.e., prefer many workers in one room over few workers in many rooms
      utility {
        val occupied = assignees.cardinality + occupants.size
        occupied * occupied
      }

      // grant access rights and notify newly selected hungry workers
      notify(assignees.selectedMembers, RoomAssignedNotification(room))
      allow(assignees.selectedMembers, "enter", room)
      allow(occupants, "enter", room)
    }

    val workroomAssignments = rules(projects.map(new WorkroomAssignment(_)))
    val lunchroomAssignments = rules(lunchrooms.map(new LunchroomAssignment(_)))

    // ensure that a worker is not assigned to more than one lunchroom
    constraints(lunchroomAssignments.map(_.assignees).allDisjoint)
  }

  val problem = root(new RoomAssignment)
}
