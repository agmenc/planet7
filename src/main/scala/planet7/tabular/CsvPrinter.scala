package planet7.tabular

import scala.collection.immutable.Seq

class CsvPrinter() {
  def top5(csv: Csv): String = {
    val top: List[Row] = csv.header +: (csv.iterator.toStream take 5).toList

    def length(row: Row): Array[Int] = row.data.map(_.length)
    def max(t: (Int, Int)): Int = t match { case (l, r) => Math.max(l,r) }
    def maxes(row1: Array[Int], row2: Array[Int]): Array[Int] = (row1 zip row2) map max

    val overallMaxes: Array[Int] = top.map(length).foldLeft(length(csv.header))(maxes)

    val dataWithSize: Seq[Array[(String, Int)]] = top.map(_.data zip overallMaxes)

    def bars(size: Int): String = if (size < 1) "" else "-" + bars(size - 1)

    val barLine: Array[(String, Int)] = overallMaxes.map(bars) zip overallMaxes

    val withHeaders: Seq[Array[(String, Int)]] = dataWithSize match {
      case head +: tail => head +: barLine +: tail
    }

    def padText(data: String, size: Int): String = data.reverse.padTo(size, ' ').reverse
    val padified: Seq[String] = withHeaders.map(r => r.map(d => padText(d._1, d._2)).mkString("  "))

    padified.mkString("\n", "\n", "\n")
  }
}