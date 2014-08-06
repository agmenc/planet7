package planet7.timing

import scala.language.dynamics

class Timer {
  var results = Map("start" -> System.currentTimeMillis())

  case class Record(description: String) {
    def apply[A](f: => A) = {
      val start = System.currentTimeMillis()
      val result = f
      val finish = System.currentTimeMillis()
      results += (description -> (finish - start), "finish" -> finish)
      result
    }
  }

  implicit class TimerEventDescription(val sc: StringContext) {
    def t(args: Any*): Record = Record(sc.parts.mkString(""))
  }

  def total = results("finish") - results("start")

  override def toString = results.toString()
}

object Timer {
  class TimingCollator(droppingFirst: Int) extends Dynamic {
    var timers: Seq[Timer] = List()

    def time: Timer = {
      timers = new Timer +: timers
      timers.head
    }

    def selectDynamic(key: String): Seq[Long] = (timers dropRight droppingFirst).map(_.results(key))

    def total: Seq[Long] = (timers dropRight droppingFirst).map(_.total)
  }

  implicit class Measurable(coll: Iterable[Long]) {
    def average: Double = coll.foldLeft(0 -> 0.0)(incrementAverage)._2

    private def incrementAverage(acc: (Int, Double), amount: Long): (Int, Double) = {
      val (count, average) = acc
      val newAverage = ((average * count) + amount)/(count + 1)
      (count + 1, newAverage)
    }
  }
}

