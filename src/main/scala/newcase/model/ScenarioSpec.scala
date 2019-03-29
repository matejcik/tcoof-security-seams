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
                       ) {
  def toPerfLine: String = {
    val roomCapacityStr =
      if (randomCapacity)
        s"rand($roomCapacity)"
      else
        roomCapacity.toString
    s"$projects, $rooms, $roomCapacityStr, $workers, $projectAssignment"
  }
}
