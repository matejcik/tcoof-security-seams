package scenario.testing

import java.io.{File, PrintWriter}
import java.lang.management.ManagementFactory
import java.time.LocalDate
import java.time.format.DateTimeFormatter

import com.sun.management.GarbageCollectionNotificationInfo
import javax.management.openmbean.CompositeData
import javax.management.{Notification, NotificationEmitter, NotificationListener}

import scala.collection.JavaConverters._
import org.chocosolver.util.tools.TimeUtils

class TestHarness[ScenarioType] {
  type ScenarioSpec = Spec[ScenarioType]

  val TEST_ROUNDS = 100
  val SOLVER_TIME_LIMIT = 30L * 1000
  val LIMIT_NANO = SOLVER_TIME_LIMIT * TimeUtils.MILLISECONDS_IN_NANOSECONDS
  val WARMUP_TIME = 10L * 1000 * TimeUtils.MILLISECONDS_IN_NANOSECONDS

  val RESULT_PATH = "results/" + LocalDate.now.format(
    DateTimeFormatter.ofPattern("YYYY-MM-dd")
  )
  new File(RESULT_PATH).mkdir()

  val logWriter = new PrintWriter(new File("lunch.log"))

  def log(s: String): Unit = synchronized {
    println(s)
    logWriter.println(s)
    logWriter.flush()
  }

  case class Measure(success: Boolean, time: Long, utility: Int)

  def formatMs(nanosec: Long): String = f"${nanosec.toDouble / 1000000}%.05f"

  class PeakMemoryListener extends NotificationListener {
    var peakMemory: Long = 0

    def reset(): Unit = peakMemory = 0
    def updatePeak(peak: Long): Unit = peakMemory = Math.max(peak, peakMemory)

    override def handleNotification(notification: Notification, o: Any): Unit = {
      if (notification.getType != GarbageCollectionNotificationInfo.GARBAGE_COLLECTION_NOTIFICATION)
        return
      val compositeData = notification.getUserData.asInstanceOf[CompositeData]
      val info = GarbageCollectionNotificationInfo.from(compositeData).getGcInfo
      val totalMemory = info.getMemoryUsageBeforeGc.asScala.values
        .map(_.getUsed)
        .sum
      updatePeak(totalMemory)
    }
  }

  def forceGc(): Unit = {
    def getGcCount: Long =
      ManagementFactory.getGarbageCollectorMXBeans.asScala
        .map(_.getCollectionCount)
        .filter(_ != -1)
        .sum

    val before = getGcCount
    System.gc()
    while (before == getGcCount) {}
  }

  // set up peak memory metric
  private val peakMemStats = ManagementFactory.getGarbageCollectorMXBeans.asScala
    .map { x =>
      val listener = new PeakMemoryListener
      x.asInstanceOf[NotificationEmitter].addNotificationListener(listener, null, null)
      listener
    }

  def measure(
      label: String,
      description: String,
      solverFunc: ScenarioSpec => Measure = solveScenario
  )(loop: (ScenarioSpec => Boolean) => Unit): Unit = {
    val filename = s"$RESULT_PATH/$label.log"
    val perfLogWriter = new PrintWriter(new File(filename))
    log(s"===== $description =====")
    log(s"saving detailed logs to $filename")

    def perf(spec: ScenarioSpec, runIndex: Int, measure: Measure, peakMemory: Long): Unit = {
      val measureStr = measure.productIterator.mkString(", ")
      perfLogWriter.println(s"${spec.toPerfLine}, $runIndex, $measureStr, $peakMemory")
      perfLogWriter.flush()
    }

    def singleRun(spec: ScenarioSpec): Boolean =
      try {
        var utility = 0
        var maxPeak: Long = 0
        val measurements = for (i <- 0 until TEST_ROUNDS) yield {
          forceGc()
          peakMemStats.foreach(_.reset())
          val m = solverFunc(spec)
          forceGc()
          val peakMemory = peakMemStats.map(_.peakMemory).sum
          maxPeak = Math.max(maxPeak, peakMemory)
          perf(spec, i, m, peakMemory)
          utility = m.utility
          m.time
        }

        val measurementsSorted = measurements.sorted
        val min = formatMs(measurements.min)
        val max = formatMs(measurements.max)
        val avg = formatMs(measurements.sum / TEST_ROUNDS)
        val med = formatMs(measurementsSorted(TEST_ROUNDS / 2))
        val maxMem = f"${maxPeak.toDouble / (1024 * 1024)}%.02f MB"
        log(
          s"Scenario $spec solved in avg $avg ms " + s" (min: $min, max: $max, med: $med), utility $utility, mem $maxMem"
        )

        measurements.exists(_ < LIMIT_NANO)
      } catch {
        case any: Throwable =>
          log(s"Scenario $spec crashed on $any")
          any.printStackTrace()
          false
      }

    loop(singleRun)
    perfLogWriter.close()
  }

  def solveScenario(spec: ScenarioSpec): Measure = {
    val model = spec.makeScenario()
    val policy = spec.policy(model)

    val start = System.nanoTime()
    policy.init()
    policy.solverLimitTime(SOLVER_TIME_LIMIT)
    while (policy.solve()) {}
    val end = System.nanoTime()
    val time = end - start

    val success = time < LIMIT_NANO
    Measure(success, time, spec.root(model).solutionUtility)
  }

  def warmup(spec: ScenarioSpec, solverFunc: ScenarioSpec => Measure = solveScenario): Unit = {
    var totalTime: Long = 0
    while (totalTime < WARMUP_TIME) totalTime += solverFunc(spec).time
    log(s"warmup completed in ${formatMs(totalTime)} ms")
  }
}
