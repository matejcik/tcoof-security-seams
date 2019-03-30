package newcase.model

object ProjectAssignment extends Enumeration {
  val ROUND_ROBIN, RANDOM = Value
}

case class ScenarioSpec(projects: Int,
                        rooms: Int,
                        roomCapacity: Int,
                        randomCapacity: Boolean,
                        workers: Int,
                        projectAssignment: ProjectAssignment.Value,
                        preassignedRooms: Int,
) {
  def toPerfLine: String = {
    s"$projects, $rooms, $roomCapacity, $randomCapacity, $workers, $projectAssignment, $preassignedRooms"
  }
}
