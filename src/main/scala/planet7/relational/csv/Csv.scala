package planet7.relational.csv

case class Csv(data: String) {
  private val allRows = data.trim.split("\n").toList
  private val header = data(allRows.head)
  private def data(s: String) = s.split(",").toList

  def rows: List[Row] = allRows.tail.filter(_.trim.nonEmpty).map(row => Row(header zip data(row)))
}

trait RowLike
case object Missing extends RowLike
case class Row(values: List[(String, String)]) extends RowLike {
  def value(key: String): Option[String] = values.find(_._1 == key).map(_._2)
}


/*

List((Row(List((ID,G), (Name,H), (Value,I))),Row(List((ID,G), (Name,X), (Value,I)))), (Row(List((ID,D), (Name,E), (Value,F))),Missing))
List((Row(List((ID,G), (Name,H), (Value,I))),Row(List((ID,G), (Name,X), (Value,I))),(Row(List((ID,D), (Name,E), (Value,F))),Missing)))

*/