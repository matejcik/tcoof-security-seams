package newcase.model

object ModelGenerator {
  def modelFromSpec(spec: ScenarioSpec): LunchModel = {
    // generate appropriate number of projects, workers and rooms
    val projects = for (i <- 0 to spec.projects)
      yield new Project(('A' + i).toString)
    val allWorkers = for (i <- 0 to spec.workers)
      yield new Worker(i, projects(i % projects.size))
    val rooms = for (i <- 0 to spec.rooms)
      yield new Room(i.toString, spec.roomCapacity)

    // generate assignments
    spec.roomUtilization match {

      case RoomUtilization.DENSE => {
        val projectWorkers =
          allWorkers.take(spec.existingAssignments).groupBy(_.project)
        var remainingRooms = rooms
        for (workers <- projectWorkers.values) {
          val roomsNeeded =
            scala.math.ceil(workers.length.toDouble / spec.roomCapacity).toInt
          val roomsTaken = remainingRooms.take(roomsNeeded)
          remainingRooms = remainingRooms.drop(roomsNeeded)
          if (roomsTaken.length < roomsNeeded)
            throw new Exception(
              "Not enough rooms to fit specified number of workers"
            )
          var remainingWorkers = workers
          for (room <- roomsTaken) {
            val workersAssigned = remainingWorkers.take(room.capacity)
            remainingWorkers = remainingWorkers.drop(room.capacity)
            workersAssigned.foreach(_.notify(RoomAssignedNotification(room)))
          }
        }
      }

      case RoomUtilization.SPARSE => for (i <- 0 to spec.existingAssignments) {
        val projectId = i % projects.size
        val roomId = i % rooms.size
        if (roomId % projects.size != projectId) {

        }
      }

    }

    new LunchModel(projects, allWorkers, rooms)
  }

  def main(args: Array[String]): Unit = {
    println("hello")
  }
}
