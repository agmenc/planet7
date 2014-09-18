package planet7.tabular

import java.util.Comparator

// TODO - CAS - 18/09/2014 - Remove SortAggregator, and just use Differs, instead.
object SortAggregator {
  def sort(csv: Csv, fieldComps: (String, Comparator[String])*): Csv = {
    val ordering: Ordering[Row] = orderBy(csv.header, fieldComps:_*)
    Csv(csv.header, csv.rows.toSeq.sorted(ordering).iterator)
  }

  def orderBy(header: Row, fieldComps: (String, Comparator[String])*): Ordering[Row] = {
    def rowComparatorFor(col: Int, comp: Comparator[String]): Comparator[Row] = new Comparator[Row] {
      override def compare(j: Row, k: Row) = comp.compare(j.data(col), k.data(col))
    }

    def equal = new Comparator[Row] {override def compare(o1: Row, o2: Row) = 0}

    implicit def rowOrderingFor(comps: Seq[Comparator[Row]]): Ordering[Row] = new Ordering[Row] {
      override def compare(x: Row, y: Row): Int = comps.find(_.compare(x, y) != 0).getOrElse(equal).compare(x, y)
    }

    def index(name: String) = header.data.indexOf(name)

    val rowComps: Seq[Comparator[Row]] = fieldComps map { case (colName, fieldComp) => rowComparatorFor(index(colName), fieldComp)}
    rowOrderingFor(rowComps)
  }
}