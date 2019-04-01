package newcase.model

import java.time.LocalTime


object ModelGenerator {
  def modelFromSpec(spec: ScenarioSpec): LunchModel = {
    val lunchrooms = for (i <- 0 until spec.lunchrooms.n)
      yield new LunchRoom(i.toString, spec.lunchrooms.capacity)

    val workrooms = for (i <- 0 until spec.workrooms.n)
      yield new WorkRoom(i.toString, spec.workrooms.capacity)

    val projects = for (i <- 0 until spec.projects) yield {
      val selectedWorkrooms =
        workrooms
        .zipWithIndex
        .collect { case (e, wi) if (wi % spec.projects) == i => e }
      new Project(('A' + i).toChar.toString, selectedWorkrooms)
    }

    val allWorkers = for (i <- 0 until spec.workers) yield {
      val w = new Worker(i, projects(i % projects.size))
      if (i < spec.hungryWorkers) w.hungry = true
      w
    }

    val roomsToPreassign = lunchrooms.take(spec.preassignedRooms)
    for ((room, worker) <- roomsToPreassign.zip(allWorkers)) {
      worker.notify(RoomAssignedNotification(room))
    }

    val model = new LunchModel(projects, allWorkers, workrooms, lunchrooms)
    if (spec.isLunchTime) model.now = LocalTime.of(13, 37)
    model
  }
}
