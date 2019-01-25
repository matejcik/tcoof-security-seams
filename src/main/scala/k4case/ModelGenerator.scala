package k4case

import java.time.{Duration, LocalDateTime}

import scala.collection.mutable
import scala.util.Random

trait ModelGenerator {
  this: TestScenario =>

  object EventsDSL {
    private val random = new Random(42)

    private class EventsBuilder {
      val events = mutable.ListBuffer.empty[ScenarioEvent]
    }

    private class PersonBehavior(val builder: EventsBuilder, val person: String, val start: LocalDateTime) {
      var timestamp = start

      def addEvent(eventType: String, minTimeSeconds: Int, maxTimeSeconds: Int, pos: Position): Unit = {
        builder.events += ScenarioEvent(timestamp, eventType, person, pos)
        timestamp = timestamp plusSeconds (minTimeSeconds + random.nextInt(maxTimeSeconds - minTimeSeconds))
      }
    }

    def move(x: Int, y: Int)(implicit pb: PersonBehavior): Unit = pb.addEvent("move", 30, 90, Position(x, y))
    def accessDoor(x: Int, y: Int)(implicit pb: PersonBehavior): Unit = pb.addEvent("access-door", 30, 90, Position(x, y))
    def accessDispenser(x: Int, y: Int)(implicit pb: PersonBehavior): Unit = pb.addEvent("access-dispenser", 30, 90, Position(x, y))

    def waitTill(ts: LocalDateTime)(implicit pb: PersonBehavior) = {
      pb.timestamp = ts
    }

    def waitRandom(min: Duration, max: Duration)(implicit pb: PersonBehavior) = {
      pb.timestamp = pb.timestamp plus min plusSeconds random.nextInt((max minus min).getSeconds().asInstanceOf[Int])
    }

    implicit class EventDuration(value: Int) {
      def seconds = Duration.ofSeconds(value)
      def minutes = Duration.ofMinutes(value)
      def hourse = Duration.ofHours(value)
    }

    def withPerson(person: String, start: LocalDateTime)(ops: PersonBehavior => Unit)(implicit builder: EventsBuilder): Unit = {
      val pb = new PersonBehavior(builder, person, start)
      ops(pb)
    }

    // start is 1 hour before the shift start
    def withWorkerInShiftA(person: String, start: LocalDateTime)(implicit builder: EventsBuilder) = withPerson(person, start) { implicit behavior =>
      move(0, 90)
      waitRandom(29 minutes, 39 minutes)

      move(0, 90)
      move(10, 90)
      accessDoor(20, 90)
      accessDispenser(30, 90)
      move(30, 80)
      move(30, 70)
      move(30, 60)
      move(30, 50)
      accessDoor(40, 50)
      move(50, 50)

      waitTill(start plusHours 9)

      waitRandom(2 minutes, 5 minutes)
      accessDoor(40, 50)
      move(30, 50)
      move(30, 60)
      move(30, 70)
      move(30, 80)
      move(30, 90)
      accessDoor(20, 90)
      move(10, 90)
      move(0, 90)
    }

    def withWorkerInShiftB(person: String, start: LocalDateTime)(implicit builder: EventsBuilder) = withPerson(person, start) { implicit behavior =>
      move(0, 90)
      waitRandom(23 minutes, 33 minutes)

      move(0, 90)
      move(10, 90)
      accessDoor(20, 90)
      accessDispenser(30, 90)
      move(40, 90)
      move(50, 90)
      move(60, 90)
      move(70, 90)
      move(80, 90)
      move(90, 90)
      move(100, 90)
      move(110, 90)
      move(110, 80)
      move(110, 70)
      move(110, 60)
      move(110, 50)
      accessDoor(120, 50)
      move(130, 50)

      waitTill(start plusHours 9)

      waitRandom(2 minutes, 5 minutes)
      accessDoor(120, 50)
      move(110, 50)
      move(110, 60)
      move(110, 70)
      move(110, 80)
      move(110, 90)
      move(100, 90)
      move(90, 90)
      move(80, 90)
      move(70, 90)
      move(60, 90)
      move(50, 90)
      move(40, 90)
      move(30, 90)
      accessDoor(20, 90)
      move(10, 90)
      move(0, 90)
    }

    def withWorkerInShiftC(person: String, start: LocalDateTime)(implicit behavior: EventsBuilder) = withPerson(person, start) { implicit behavior =>
      move(0, 90)
      waitRandom(25 minutes, 35 minutes)

      move(0, 90)
      move(10, 90)
      accessDoor(20, 90)
      accessDispenser(30, 90)
      move(40, 90)
      move(50, 90)
      move(60, 90)
      move(70, 90)
      move(80, 90)
      move(90, 90)
      move(100, 90)
      move(110, 90)
      move(110, 100)
      accessDoor(120, 100)
      move(130, 100)

      waitTill(start plusHours 9)

      waitRandom(2 minutes, 5 minutes)
      accessDoor(120, 100)
      move(110, 100)
      move(110, 90)
      move(100, 90)
      move(90, 90)
      move(80, 90)
      move(70, 90)
      move(60, 90)
      move(50, 90)
      move(40, 90)
      move(30, 90)
      accessDoor(20, 90)
      move(10, 90)
      move(0, 90)
    }

    def withEvents(ops: EventsBuilder => Unit): List[ScenarioEvent] = {
      val builder = new EventsBuilder
      ops(builder)
      builder.events.toList
    }
  }

  object ModelDSL {

    private class ModelBuilder {
      val workersMap = mutable.Map.empty[String, Worker]
      val factoriesMap = mutable.Map.empty[String, Factory]
      val shiftsMap = mutable.Map.empty[String, Shift]
      val workPlacesMap = mutable.Map.empty[String, WorkPlace]
    }

    private class FactoryScope(val builder: ModelBuilder, val factoryId: String, val offsetX: Int, val offsetY: Int) {
      private def init(): Unit = {
        val positions = for {
          x <- (30 + offsetX).to(170 + offsetX, 10)
          y <- (10 + offsetY).to(110 + offsetY, 10)
        } yield Position(x, y)

        val workplaces = List(
          new WorkPlace(
            factoryId + "-A",
            List(Position(50 + offsetX, 50 + offsetY)),
            new Door(factoryId + "-door", Position(40 + offsetX, 50 + offsetY))
          ),
          new WorkPlace(
            factoryId + "-B",
            List(Position(130 + offsetX, 50 + offsetY)),
            new Door(factoryId + "-door", Position(120 + offsetX, 50 + offsetY))
          ),
          new WorkPlace(
            factoryId + "-C",
            List(Position(130 + offsetX, 100 + offsetY)),
            new Door(factoryId + "-door", Position(120 + offsetX, 100 + offsetY))
          )
        )

        builder.workPlacesMap ++= workplaces.map(wp => (wp.id, wp))

        builder.factoriesMap(factoryId) = new Factory(
          factoryId,
          positions.toList,
          new Door(factoryId + "-door", Position(20 + offsetX, 90 + offsetY)),
          new Dispenser(factoryId + "-dispenser", Position(30 + offsetX, 90 + offsetY)),
          workplaces
        )
      }

      def addWorker(id: String, caps: Set[String]): Unit = {
        builder.workersMap(id) = new Worker(id, Position(0 + offsetX, 90 + offsetY), caps, false)
      }

      def addShift(id: String, startTime: LocalDateTime, endTime: LocalDateTime, workPlace: String, foreman: String, workers: List[String], standbys: List[String], assignments: Map[String, String]): Unit = {
        val gid = factoryId + "-" + id

        builder.shiftsMap(gid) = new Shift(
          gid,
          startTime,
          endTime,
          builder.workPlacesMap(factoryId + "-" + workPlace),
          builder.workersMap(foreman),
          workers.map(wrk => builder.workersMap(wrk)),
          standbys.map(wrk => builder.workersMap(wrk)),
          assignments.map(keyVal => (builder.workersMap(keyVal._1) -> keyVal._2))
        )
      }

      init()
    }


    def withFactory(id: String, offsetX: Int, offsetY: Int)(ops: FactoryScope => Unit)(implicit builder: ModelBuilder): Unit = {
      val ms = new FactoryScope(builder, id, offsetX, offsetY)
      ops(ms)
    }

    def withWorker(id: String, caps: Set[String])(implicit ms: FactoryScope): Unit = ms.addWorker(id, caps)

    def withShift(id: String, startTime: LocalDateTime, endTime: LocalDateTime, workPlace: String, foreman: String, workers: List[String], standbys: List[String], assignments: Map[String, String])(implicit ms: FactoryScope): Unit =
      ms.addShift(id, startTime, endTime, workPlace, foreman, workers, standbys, assignments)


    def withModel(ops: ModelBuilder => Unit): (Map[String, Worker], Map[String, Factory], Map[String, Shift]) = {
      val builder = new ModelBuilder
      ops(builder)

      (builder.workersMap.toMap, builder.factoriesMap.toMap, builder.shiftsMap.toMap)
    }
  }
}

