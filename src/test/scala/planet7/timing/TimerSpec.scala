package planet7.timing

import org.scalatest.{MustMatchers, WordSpec}

import scala.collection.mutable.ListBuffer

class TimerSpec extends WordSpec with MustMatchers {
  "We can calculate the average of some times" in {
    val times: Seq[Long] = Seq(30, 40, 50, 60)

    assert(times.average === 45)
  }

  "We can name and time some snippets of code" in {
    val timer = new Timer
    import timer._

    t"dig a hole" { Thread.sleep(50) }
    t"fill it up again" { Thread.sleep(75) }

    timer.`dig a hole`.last must be >= 50L
    timer.`fill it up again`.last must be >= 75L
  }

  "We can measure nested statements" in {
    val timer = new Timer
    import timer._

    t"total" {
      t"dig a hole" {Thread.sleep(50)}
      t"fill it up again" {Thread.sleep(75)}
    }

    timer.total.last must be >= 125L
    timer.total.last must be < 250L
  }

  "We can time multiple runs across named code snippets and find the average" in {
    val timer = new Timer(4)
    import timer._

    for (i <- 1 to 20) t"total" {
      t"dig" {Thread.sleep(50)}
      t"fill" {Thread.sleep(75)}
    }

    timer.dig.average must be < timer.fill.average
    timer.total.average must be > 125.0
  }

  "We drop the first N items of any sequence to show JIT-optimised times" in {
    val timer = new Timer(3) {
      results = Map("fetchWater" -> ListBuffer(100L, 80L, 60L, 40L, 20L, 0L))
    }

    assert(timer.fetchWater.average === 20)
  }

  "toString is sorted by descending average values and formatted adequately" in {
    val timer = new Timer(2) {
      results = Map(
        "medium" -> ListBuffer(50L, 50L, 50L, 50L, 50L, 50L),
        "high" -> ListBuffer(200L, 200L, 200L, 200L),
        "really very quick" -> ListBuffer(4L, 4L, 4L, 4L, 4L,4L, 4L, 4L, 4L)
      )
    }

    assert(timer.toString === """
                                |
                                |                   high       200.00 ms (avg. of 2 readings)
                                |                 medium        50.00 ms (avg. of 4 readings)
                                |      really very quick         4.00 ms (avg. of 7 readings)
                                |
                                |""".stripMargin)
  }
}