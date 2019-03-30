package newcase.model

import tcof._


class Room(name: String, val capacity: Int) extends Component {
  name(s"Room:$name")
}


class Project(name: String) extends Component {
  name(s"Project:$name")
}


class Worker(id: Int, val project: Project) extends Component {
  name(s"Worker:$id:${project.name}")
}


case class RoomAssignedNotification(room: Room) extends Notification


class LunchModel(val projects: Seq[Project],
                 val workers: Seq[Worker],
                 val rooms: Seq[Room])
    extends Model {

  class LunchProblem extends RootEnsemble {
    name("match hungry workers to free lunchrooms")

    val unassigned =
      workers.filterNot(_.notified[RoomAssignedNotification])

    class RoomAssignment(room: Room) extends Ensemble {
      name(s"assign workers to room ${room.name}")
      val occupants =
        workers.filter(_.notified(RoomAssignedNotification(room)))

      val freeSpaces = room.capacity - occupants.size
      val assignees = subsetOf(unassigned, _ <= freeSpaces)

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

    val roomAssignments = rules(rooms.map(new RoomAssignment(_)))

    constraints(roomAssignments.map(_.assignees).allDisjoint)
//    utility(roomAssignments.sum(_.utility))
  }

  val problem = root(new LunchProblem)
}
