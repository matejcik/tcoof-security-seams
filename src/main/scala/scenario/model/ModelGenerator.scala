package scenario.model

import java.time.LocalTime
import scala.collection.mutable.{Map => MutableMap}


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

    val workers = for (i <- 0 until spec.workers)
      yield new Worker(i, projects(i % projects.size))

    val _groupByProject = workers.groupBy(_.project)
    val workersByProject = MutableMap(
      projects.map(p => p -> _groupByProject.getOrElse(p, Seq.empty)): _*
    )

    if (spec.fillRooms) {
      val loopProjects = Stream.continually(projects).flatten
      for ((room, project) <- lunchrooms zip loopProjects) {
        val projectWorkers = workersByProject(project)
        projectWorkers.take(room.capacity).foreach(
          _.notify(RoomAssignedNotification(room))
        )
        workersByProject(project) = projectWorkers.drop(room.capacity)
      }
    }

    workers
      .filterNot(_.notified[RoomAssignedNotification])
      .take(spec.hungryWorkers)
      .foreach(_.hungry = true)

    val model = new LunchModel(projects, workers, workrooms, lunchrooms)
    if (spec.isLunchTime) model.now = LocalTime.of(13, 37)
    model
  }
}
