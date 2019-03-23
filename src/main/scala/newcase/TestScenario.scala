package newcase

import java.io.{File, PrintWriter}
import java.time.LocalDateTime

import tcof._
import scala.util.control.Breaks._

class TestScenario extends Model {

  abstract class Room(val room_name: String, val capacity: Int)
      extends Component {
    name(room_name)
  }

  class WorkRoom(name: String, capacity: Int) extends Room(name, capacity)
  class LunchRoom(name: String, capacity: Int) extends Room(name, capacity)

  sealed trait WorkerStatus
  case object WorkerWorking extends WorkerStatus
  case object WorkerHungry extends WorkerStatus

  class Worker(name: String, val status: WorkerStatus) extends Component

  class Project(name: String, val workers: Seq[Worker]) extends Component

  val allWorkers = for (i <- 1 to 10)
    yield new Worker(s"Worker-{i}", WorkerHungry)

  val lunchrooms =
    List(new LunchRoom("Lunchroom 1", 10), new LunchRoom("Lunchroom 2", 10))

  val projects = List(
    new Project("Project A", allWorkers.take(5)),
    new Project("Project B", allWorkers.drop(5)),
  )

  class Everybody extends RootEnsemble {
    name("everybody in the building")

    class LunchGroup(room: LunchRoom) extends Ensemble {
      val project = oneOf(projects)
      val lunchers = _addRole(
        "lunchers"+room.name,
        allWorkers.filter(_.status == WorkerHungry),
        c => c < room.capacity
      )

      constraints {
        project.all(p => lunchers.all(p.workers.contains(_)))
      }

      allow(lunchers.selectedMembers, "enter", room)
    }

    val lunchGroups = rules(lunchrooms.map(new LunchGroup(_)))

    constraints {
      lunchGroups.map(_.lunchers).allDisjoint
    }
  }

  case class LunchroomAssignedNotification(worker: Worker, room: LunchRoom)
      extends Notification

  val everybody = root(new Everybody)
}

object TestScenario {
  def main(args: Array[String]): Unit = {
    val scenario = new TestScenario
    scenario.everybody.init()
    scenario.everybody.solve()
    if (scenario.everybody.exists) {
      scenario.everybody.commit()
      for (action <- scenario.everybody.actions) {
        println(action)
      }
    } else {
      println("no solution found :(")
    }
  }
}
