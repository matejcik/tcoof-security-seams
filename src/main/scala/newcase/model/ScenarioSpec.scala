package newcase.model

object RoomUtilization extends Enumeration {
  val DENSE, SPARSE, RANDOM = Value
}

case class ScenarioSpec(rooms: Int,
                        roomCapacity: Int,
                        projects: Int,
                        workers: Int,
                        existingAssignments: Int,
                        roomUtilization: RoomUtilization.Value,
)
