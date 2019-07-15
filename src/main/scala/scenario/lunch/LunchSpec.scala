package scenario.lunch

import java.time.LocalTime

import cz.cuni.mff.d3s.trust._

import scala.language.implicitConversions
import scenario.testing.Spec

import scala.collection.mutable.{Map => MutableMap}

case class RoomParam(n: Int, capacity: Int) {
  override def toString: String = s"($n,$capacity)"
}

case class LunchSpec(
    projects: Int,
    lunchrooms: RoomParam,
    workrooms: RoomParam,
    workers: Int,
    hungryWorkers: Int,
    fillRooms: Int,
    isLunchTime: Boolean
) extends Spec[LunchScenario] {
  require(workers >= hungryWorkers)

  override def toPerfLine: String = {
    s"$projects, ${lunchrooms.n}, ${lunchrooms.capacity}, ${workrooms.n}, " +
      s"${workrooms.capacity}, $workers, $hungryWorkers, $fillRooms, $isLunchTime"
  }

  def makeScenario(): LunchScenario = {
    val newLunchrooms = for (i <- 0 until lunchrooms.n)
      yield new LunchRoom(i.toString, lunchrooms.capacity)

    val newWorkrooms = for (i <- 0 until workrooms.n)
      yield new WorkRoom(i.toString)

    val newProjects = for (i <- 0 until projects) yield {
      val selectedWorkrooms =
        newWorkrooms.zipWithIndex
          .collect { case (e, wi) if (wi % projects) == i => e }
      Project(('A' + i).toChar.toString, selectedWorkrooms)
    }

    val newWorkers = for (i <- 0 until workers)
      yield new Worker(i, newProjects(i % newProjects.size))

    val _groupByProject = newWorkers.groupBy(_.project)
    val workersByProject = MutableMap(
      newProjects.map(p => p -> _groupByProject.getOrElse(p, Seq.empty)): _*
    )

    val loopProjects = Stream.continually(newProjects).flatten
    for ((room, project) <- newLunchrooms zip loopProjects) {
      val projectWorkers = workersByProject(project)
      projectWorkers
        .take(fillRooms)
        .foreach(_.location = Some(room))
      workersByProject(project) = projectWorkers.drop(fillRooms)
    }

    newWorkers
      .filter(_.location.isEmpty)
      .take(hungryWorkers)
      .foreach(_.hungry = true)

    val model =
      new LunchScenario(newProjects, newWorkers, newWorkrooms, newLunchrooms)
    if (isLunchTime) model.now = LocalTime.of(13, 37)
    model
  }

  override def policy(scenario: LunchScenario): Policy[_] = scenario.policy
}

object LunchSpec {
  implicit def tupleToRoomParam(tuple: (Int, Int)): RoomParam =
    RoomParam(tuple._1, tuple._2)
}
