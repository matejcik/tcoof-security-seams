package newcase.model

import java.time.{LocalDate, LocalDateTime, LocalTime}

import tcof._

class Room(name: String, val capacity: Int) extends Component {
  name(s"Room:$name")
}

class LunchRoom(name: String, capacity: Int) extends Room(name, capacity)
class WorkRoom(name: String, capacity: Int) extends Room(name, capacity)

class Project(name: String, val workrooms: Seq[WorkRoom]) extends Component {
  name(s"Project:$name")
}

class Worker(id: Int, val project: Project) extends Component {
  name(s"Worker:$id:${project.name}")
  var hungry = false
}

case class RoomAssignedNotification(room: Room) extends Notification

class LunchModel(val projects: Seq[Project],
                 val workers: Seq[Worker],
                 val workrooms: Seq[WorkRoom],
                 val lunchrooms: Seq[LunchRoom],
) extends Model {

  val BUILDING_OPEN_TIME = LocalTime.of(7, 30)
  val BUILDING_CLOSE_TIME = LocalTime.of(21, 0)
  val LUNCH_OPEN_TIME = LocalTime.of(11, 30)
  val LUNCH_CLOSE_TIME = LocalTime.of(15, 0)

  val DEFAULT_NOW = LocalTime.of(8, 42)
  var now = DEFAULT_NOW

  class RoomAssignment extends RootEnsemble {
    name("assign workers to projects and rooms")

    private val _workersGroupByProject = workers.groupBy(_.project)
    val workersByProject =
      projects.map(p => p -> _workersGroupByProject.getOrElse(p, Seq.empty)).toMap

    val hungryWorkers =
      workers.filter(_.hungry).filterNot(_.notified[RoomAssignedNotification])

    class WorkroomAssignment(project: Project) extends Ensemble {
      name(s"allow workers on project ${project.name} to enter assigned rooms")

      situation {
        (now isAfter BUILDING_OPEN_TIME) && (now isBefore BUILDING_CLOSE_TIME)
      }

      allow(workersByProject(project), "enter", project.workrooms)
    }

    class LunchroomAssignment(room: LunchRoom) extends Ensemble {
      name(s"assign workers to room ${room.name}")

      situation {
        (now isAfter LUNCH_OPEN_TIME) && (now isBefore LUNCH_CLOSE_TIME)
      }

      val occupants =
        workers.filter(_.notified(RoomAssignedNotification(room)))

      val freeSpaces = room.capacity - occupants.size
      val assignees = subsetOf(hungryWorkers, _ <= freeSpaces)

      val project = oneOf(projects)

      constraints {
        project.all(p => assignees.all(_.project == p)) &&
        project.all(p => occupants.forall(_.project == p))
      }

      utility {
        val occupied = assignees.cardinality + occupants.size
        occupied * occupied
      }

      notify(assignees.selectedMembers, RoomAssignedNotification(room))
      allow(assignees.selectedMembers, "enter", room)
      allow(occupants, "enter", room)
    }

    val workroomAssignments = rules(projects.map(new WorkroomAssignment(_)))
    val lunchroomAssignments = rules(lunchrooms.map(new LunchroomAssignment(_)))

    constraints(lunchroomAssignments.map(_.assignees).allDisjoint)
  }

  val problem = root(new RoomAssignment)
}
