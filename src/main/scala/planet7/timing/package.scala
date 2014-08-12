package planet7

package object timing {
  implicit class Measurable(coll: Iterable[Long]) {
    def average: Double = coll.foldLeft(0 -> 0.0)(incrementAverage)._2

    private def incrementAverage(acc: (Int, Double), amount: Long): (Int, Double) = {
      val (count, average) = acc
      val newAverage = ((average * count) + amount)/(count + 1)
      (count + 1, newAverage)
    }
  }
}
