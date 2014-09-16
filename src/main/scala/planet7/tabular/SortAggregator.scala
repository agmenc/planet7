package planet7.tabular

import java.util.Comparator

object SortAggregator {
  def sort(csv: Csv, fieldComps: (String, Comparator[String])*): Csv = {
    def rowComparatorFor(col: Int, comp: Comparator[String]): Comparator[Row] = new Comparator[Row] {
      override def compare(j: Row, k: Row) = comp.compare(j.data(col), k.data(col))
    }

    def equal = new Comparator[Row] { override def compare(o1: Row, o2: Row) = 0 }

    implicit def rowOrderingFor(comps: Seq[Comparator[Row]]) = new Ordering[Row] {
      override def compare(x: Row, y: Row): Int = comps.find(_.compare(x, y) != 0).getOrElse(equal).compare(x, y)
    }

    def index(name: String) = csv.header.data.indexOf(name)

    val rowComps: Seq[Comparator[Row]] = fieldComps map { case (colName, fieldComp) => rowComparatorFor(index(colName), fieldComp) }
    Csv(csv.header, csv.rows.toSeq.sorted(rowOrderingFor(rowComps)).iterator)
  }
}