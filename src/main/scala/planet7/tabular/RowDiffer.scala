package planet7.tabular

import java.util.Comparator

case class RowDiffer(header: Row, fieldComps: (String, Comparator[String])*) extends Differentiator[Row] {
  override def zero = EmptyRow
  override def ordering = orderBy(header, fieldComps:_*)

  // TODO - CAS - 18/09/2014 - Extract this into a general Ordering[T] creator
  private def orderBy(header: Row, fieldComps: (String, Comparator[String])*): Ordering[Row] = {
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

object EmptyRow extends Row(Array.empty[String], Nil)

case object FieldDiffer extends Differentiator[(String, String)] {
  override def zero = ("", "")
  override def ordering = new Ordering[(String, String)] {
    override def compare(x: (String, String), y: (String, String)) = x._1 compare y._1
  }

  def prettyPrint(fieldDiffs: Seq[((String, String), (String, String))]): Seq[String] = fieldDiffs.map {
    case (left, right) => s"${left._1}: ${left._2} -> ${right._2}"
  }
}

case object StringDiffer extends Differentiator[String] {
  override def zero = ""

  override def ordering = Ordering.String
}

object NaiveRowDiffer extends Differentiator[Row] {
  override def zero = EmptyRow
  override def ordering = NaiveRowOrdering
}

object NaiveRowOrdering extends Ordering[Row] {
  override def compare(x: Row, y: Row) = x.data.mkString compare y.data.mkString
}