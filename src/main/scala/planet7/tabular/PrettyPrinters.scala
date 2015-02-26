package planet7.tabular

import scala.collection.immutable.Seq

trait PrettyPrinters {

  def print(csv: Csv): String = {
    val top: Seq[Row] = csv.header +: (csv.iterator.toStream take 5).toSeq

    def length(row: Row): Array[Int] = row.data.map(_.length)

    def max(row1: Array[Int], row2: Array[Int]): Array[Int] = row1

    val overallMaxes = top.map(length).foldLeft(length(csv.header))(max)

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