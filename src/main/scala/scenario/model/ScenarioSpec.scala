package scenario.model

import scala.language.implicitConversions

object ProjectAssignment extends Enumeration {
  val ROUND_ROBIN, RANDOM = Value
}

case class RoomParam(n: Int, capacity: Int) {
  override def toString: String = s"($n,$capacity)"
}

case class ScenarioSpec(projects: Int,
                        lunchrooms: RoomParam,
                        workrooms: RoomParam,
                        workers: Int,
                        hungryWorkers: Int,
                        fillRooms: Boolean,
                        isLunchTime: Boolean) {
  require(workers >= hungryWorkers)

  def toPerfLine: String = {
    s"$projects, ${lunchrooms.n}, ${lunchrooms.capacity}, ${workrooms.n}, " +
    s"${workrooms.capacity}, $workers, $hungryWorkers, $fillRooms, $isLunchTime"
  }
}

object ScenarioSpec {
  implicit def tupleToRoomParam(tuple: (Int, Int)): RoomParam = RoomParam(tuple._1, tuple._2)
}
