package planet7.tabular

import java.io.Serializable

import scala.collection.immutable.Seq

trait PrintingImplicits {

  // Needs to be built in Csv???

  // converts its DataSource to a CircularDataSource
  // always has 5 lines after the last step (unless datasource had zero lines)
  def print(csv: Csv): String = {
    // Use header to get expected columns
    // Track the max sizes for each column

    val streamerator = csv.iterator.toStream
    val top: Seq[Row] = (streamerator take 5).toSeq

    def maxes(row: Row): Array[Int] = row.data.map(_.length)

    // ???
    def max(row1: Array[Int], row2: Array[Int]): Array[Int] = row1

    // ???
    val overallMaxes = top.map(maxes).foldLeft(maxes(csv.header))(max)

    val tuples: Seq[Array[(String, Int)]] = top.map(_.data zip overallMaxes)

    def bars(size: Int): String = if (size < 1) "" else "-" + bars(size - 1)

    val barLine: Array[(String, Int)] = overallMaxes.map(bars) zip overallMaxes

    val withHeaders: Seq[Array[(String, Int)]] = (csv.header.data zip overallMaxes) +: barLine +: tuples

    def padText(data: String, size: Int): String = data
    val padified: Seq[String] = withHeaders.map(r => r.map(d => padText(d._1, d._2)).mkString("  "))

    val result = padified.mkString("\n")
    println(result)
    result
  }

  def loop(t: TabularDataSource): TabularDataSource = {
    val stream: Stream[Row] = t.rows.toStream
    stream.force
    ???
  }
}