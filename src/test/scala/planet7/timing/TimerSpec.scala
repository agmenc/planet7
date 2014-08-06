package planet7.timing

import org.scalatest.WordSpec
import planet7.timing.Timer._

class TimerSpec extends WordSpec {
  "We can calculate the average of some times" in {
    val times: Seq[Long] = Seq(30, 40, 50, 60)

    assert(times.average === 45)
  }
}