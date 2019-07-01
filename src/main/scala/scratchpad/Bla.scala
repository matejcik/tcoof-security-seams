package scratchpad
import scala.collection.mutable.ListBuffer

class DelayedInt(val value: Int) {
  var locked = true
}

object DelayedInt {
  implicit def delayedIntToInt(del: DelayedInt) = {
    if (del.locked) throw new RuntimeException("not yet!")
    del.value
  }
}

object Main {
  var queue: Seq[() => Int] = Seq.empty

  def queueInt(int: => Int): Unit = {
    queue :+= int _
  }

  def printQueue(): Unit =
    for (f <- queue)
      println("int is: " + f.apply())

  def main(args: Array[String]): Unit = {
    val di = new DelayedInt(42)
    // println(5 + di) // throws exception
    queueInt(6 + di) // also throws exception
    queueInt(di) // OK
    queueInt(di) // OK
    di.locked = false
    printQueue()
  }
}