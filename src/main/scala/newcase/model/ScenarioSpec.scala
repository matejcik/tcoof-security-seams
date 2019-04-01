package newcase.model

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
                        preassignedRooms: Int,
                        isLunchTime: Boolean,
                       ) {
  require(workers >= hungryWorkers)
  require(lunchrooms.n >= preassignedRooms)

  def toPerfLine: String = {
    s"$projects, ${lunchrooms.n}, ${lunchrooms.capacity}, ${workrooms.n}, ${
      workrooms.capacity
    }, $workers, $hungryWorkers, $preassignedRooms, $isLunchTime"
  }
}

object ScenarioSpec {
  implicit def tupleToRoomParam(tuple: (Int, Int)): RoomParam = RoomParam(tuple._1, tuple._2)
}
