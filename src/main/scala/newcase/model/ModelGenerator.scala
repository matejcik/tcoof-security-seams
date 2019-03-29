package newcase.model

import scala.util.Random

object ModelGenerator {
  def modelFromSpec(spec: ScenarioSpec): LunchModel = {
    // generate appropriate number of projects, workers and rooms
    val projects = for (i <- 0 until spec.projects)
      yield new Project(('A' + i).toChar.toString)

    val allWorkers = for (i <- 0 until spec.workers) yield {
      val project =
        spec.projectAssignment match {
          case ProjectAssignment.ROUND_ROBIN => i % projects.size
          case ProjectAssignment.RANDOM => Random.nextInt(projects.size)
        }
      new Worker(i, projects(project))
    }

    val rooms = for (i <- 0 until spec.rooms) yield {
      val capacity =
        if (spec.randomCapacity)
          Random.nextInt(spec.roomCapacity) + 1
        else
          spec.roomCapacity
      new Room(i.toString, capacity)
    }

    //    // generate assignments
    //    spec.roomUtilization match {
    //
    //      case RoomUtilization.DENSE => {
    //        val projectWorkers =
    //          allWorkers.take(spec.existingAssignments).groupBy(_.project)
    //        var remainingRooms = rooms
    //        for (workers <- projectWorkers.values) {
    //          val roomsNeeded =
    //            scala.math.ceil(workers.length.toDouble / spec.roomCapacity).toInt
    //          val roomsTaken = remainingRooms.take(roomsNeeded)
    //          remainingRooms = remainingRooms.drop(roomsNeeded)
    //          if (roomsTaken.length < roomsNeeded)
    //            throw new Exception(
    //              "Not enough rooms to fit specified number of workers"
    //            )
    //          var remainingWorkers = workers
    //          for (room <- roomsTaken) {
    //            val workersAssigned = remainingWorkers.take(room.capacity)
    //            remainingWorkers = remainingWorkers.drop(room.capacity)
    //            workersAssigned.foreach(_.notify(RoomAssignedNotification(room)))
    //          }
    //        }
    //      }
    //
    //      case RoomUtilization.SPARSE => for (i <- 0 to spec.existingAssignments) {
    //        val projectId = i % projects.size
    //        val roomId = i % rooms.size
    //        if (roomId % projects.size != projectId) {
    //
    //        }
    //      }
    //
    //    }

    new LunchModel(projects, allWorkers, rooms)
  }

  def main(args: Array[String]): Unit = {
    println("hello")
  }
}
