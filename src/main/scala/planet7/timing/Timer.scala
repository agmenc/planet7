package planet7.timing

import scala.collection.mutable.ListBuffer
import scala.language.dynamics

class Timer(droppingFirst: Int = 0) extends Dynamic {
  var results: Map[String, ListBuffer[Long]] = Map()

  def apply[A](f: => A) = Record("time")(f)

  def selectDynamic(key: String): Seq[Long] = trim(results(key))

  case class Record(description: String) {
    def apply[A](f: => A) = {
      val start = System.currentTimeMillis()
      val result = f
      if (!results.contains(description)) results += (description -> ListBuffer())
      results(description) += (System.currentTimeMillis() - start)
      result
    }
  }

  // TODO - CAS - 12/08/2014 - Also support a t("monkeys") method, since I am told by Kevin that this violates the principle of least surprise
  implicit class TimerStringInterpolator(val sc: StringContext) {
    def t(args: Any*): Record = Record(sc.s(args:_*))
  }

  override def toString = results.map{ case (key, timings) => s"$key: ${trim(timings).average} ms (average of ${trim(timings).size})" }.mkString("\n")

  private def trim(buf: ListBuffer[Long]) = buf drop droppingFirst
}