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

    val roomsToPreassign = rooms.take(spec.preassignedRooms)
    for ((room, worker) <- roomsToPreassign.zip(allWorkers)) {
      worker.notify(RoomAssignedNotification(room))
    }

    new LunchModel(projects, allWorkers, rooms)
  }
}
