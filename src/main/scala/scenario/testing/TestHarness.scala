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

/** Measuring toolkit and mini-DSL.
  *
  * Unifies writing evaluation test-cases and saving the results of performance runs.
  * @tparam ScenarioType Concrete scenario type
  */
class TestHarness[ScenarioType] {
  type ScenarioSpec <: Spec[ScenarioType]

  /** Number of rounds for each configuration. */
  val TEST_ROUNDS = 100

  /** Solver time limit in milliseconds. */
  val SOLVER_TIME_LIMIT = 30L * 1000

  /** Solver time limit in nanoseconds.
    *
    * Needed when working with results of `System.nanoTime`.
    */
  def LIMIT_NANO = SOLVER_TIME_LIMIT * TimeUtils.MILLISECONDS_IN_NANOSECONDS

  /** Wall-clock length of warmup runs, in nanoseconds. */
  val WARMUP_TIME = 10L * 1000 * TimeUtils.MILLISECONDS_IN_NANOSECONDS

  /** Path to a directory with result logs. */
  val RESULT_PATH = "results/" + LocalDate.now.format(
    DateTimeFormatter.ofPattern("YYYY-MM-dd")
  )
  new File(RESULT_PATH).mkdir()

  /** Summary logging. */
  val logWriter = new PrintWriter(new File("lunch.log"))

  /** Log to console and the summary log. */
  def log(s: String): Unit = synchronized {
    println(s)
    logWriter.println(s)
    logWriter.flush()
  }

  /** Result of a single test run. */
  case class Measure(success: Boolean, time: Long, utility: Int)

  /** Convert nanoseconds to millisecond string. */
  def formatMs(nanosec: Long): String = f"${nanosec.toDouble / 1000000}%.05f"

  /** Listener for GC events. */
  class PeakMemoryListener extends NotificationListener {
    var peakMemory: Long = 0

    def reset(): Unit = peakMemory = 0
    def updatePeak(peak: Long): Unit = peakMemory = math.max(peak, peakMemory)

    /** Record peak memory before last GC run. */
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

  /** Attempt to force garbage collection.
    *
    * Finds the current GC counter, call `System.gc()`, and waits until
    * the counter increases.
    */
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

  /** Peak memory listener collection. */
  private val peakMemStats = ManagementFactory.getGarbageCollectorMXBeans.asScala
    .map { x =>
      val listener = new PeakMemoryListener
      x.asInstanceOf[NotificationEmitter].addNotificationListener(listener, null, null)
      listener
    }

  /** Create a measurement test case.
    *
    * Usage:
    * {{{
    *   measure(
    *     "sometest",
    *     "testing some performance properties
    *   ) { m =>
    *     val smallSpec: ScenarioSpec = /* ... */
    *     warmup(smallSpec)
    *     for (config <- allConfigurationsOfThisTest)
    *       m(config)
    *   }
    * }}}
    *
    * @param label Name of the result file
    * @param description Human readable description
    * @param solverFunc Optional custom function to perform and measure one run
    * @param loop Block of code that iterates over configurations and invokes
    *             the measurement function on each.
    */
  def measure(
      label: String,
      description: String,
      solverFunc: ScenarioSpec => Measure = solveScenario
  )(loop: (ScenarioSpec => Boolean) => Unit): Unit = {
    val filename = s"$RESULT_PATH/$label.log"
    val perfLogWriter = new PrintWriter(new File(filename))
    log(s"===== $description =====")
    log(s"saving detailed logs to $filename")

    /** Write a line to the detailed performance log. */
    def perf(spec: ScenarioSpec, runIndex: Int, measure: Measure, peakMemory: Long): Unit = {
      val measureStr = measure.productIterator.mkString(", ")
      perfLogWriter.println(s"${spec.toPerfLine}, $runIndex, $measureStr, $peakMemory")
      perfLogWriter.flush()
    }

    /** Test a single configuration.
      *
      * Repeats the configuration `TEST_ROUNDS` times and records results.
      *
      * @param spec Scenario spec
      * @return true if at least one test run succeeded.
      */
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
          maxPeak = math.max(maxPeak, peakMemory)
          perf(spec, i, m, peakMemory)
          utility = math.max(utility, m.utility)
          m.time
        }

        val min = formatMs(measurements.min)
        val max = formatMs(measurements.max)
        val avg = formatMs(measurements.sum / TEST_ROUNDS)
        val med = formatMs(measurements.sorted.apply((TEST_ROUNDS / 2)))
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

  /** Default solver function.
    *
    * Generates an instance of the scenario, starts the timer, invokes `resolve()`,
    * stops the timer, and reports if the search succeeded under time limit.
    */
  def solveScenario(spec: ScenarioSpec): Measure = {
    val model = spec.makeScenario()
    val policy = spec.policy(model)

    val start = System.nanoTime()
    policy.resolve(SOLVER_TIME_LIMIT)
    val end = System.nanoTime()
    val time = end - start

    val success = policy.exists && time < LIMIT_NANO
    val utility = if (policy.exists) policy.solutionUtility else -1

    Measure(success, time, utility)
  }

  /** Warm up the JIT
    *
    * Runs the solver function repeatedly on the provided spec, until `WARMUP_TIME`
    * expires. By default, `WARMUP_TIME` is 10 s, which is enough for the JVM JIT
    * to precompile hot paths.
    *
    * @param spec Scenario spec
    * @param solverFunc Optional custom solver function
    */
  def warmup(spec: ScenarioSpec, solverFunc: ScenarioSpec => Measure = solveScenario): Unit = {
    var totalTime: Long = 0
    while (totalTime < WARMUP_TIME) totalTime += solverFunc(spec).time
    log(s"warmup completed in ${formatMs(totalTime)} ms")
  }
}
