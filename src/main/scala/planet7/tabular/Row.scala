package planet7.tabular

case class Row(data: Array[String], validationFailures: Seq[String] = Nil) extends Iterable[String] {
  override def toString = data.mkString(",")

  def isInvalid = validationFailures != Nil && validationFailures.nonEmpty

  def apply(index: Int): String = this.data(index)

  // TODO - CAS - 20/02/15 - Given the header, we should be able to extract a column by name
//  def apply(header: Row, column: String): String = this.data(index)

  override def canEqual(that: Any) = that.isInstanceOf[Row]

  override def equals(that: Any) = that match {
    case thatRow: Row => this.data.deep == thatRow.data.deep
    case _ => false
  }

  override def iterator = data.iterator
}

object Row {
  def indexOf(header: Row, column: String): Int = header.data.indexOf(column) match {
    case notFound if notFound < 0 => throw new ColumnDoesNotExistException(column, header)
    case ok => ok
  }

  def showDiffs(left: Row, right: Row): String = matchPositions(left, right) match {
    case (l, r) => s"""
                   |$left
                   |$right
                   |""".stripMargin
  }


  def matchPositions(left: Row, right: Row): (Row, Row) = {
    val x: Seq[(String, String)] = matchPositions(left.data.toSeq, right.data.toSeq, Nil)
    val y: (Seq[String], Seq[String]) = x.unzip

    y match {
      case (l, r) => (Row(l.toArray), Row(r.toArray))
    }
  }

  def matchPositions(left: Seq[String], right: Seq[String], acc: Seq[(String, String)]): Seq[(String, String)] = {
    (left, right) match {
      case (Nil, Nil) => acc
      case (Nil, rh +: rt) => (Seq("*") zipAll (right, "*", "*")) ++: acc
      case (lh +: lt, Nil) => (left zipAll (Seq("*"), "*", "*")) ++: acc
      case (lh +: lt, rh +: rt) if lh == rh => matchPositions(lt, rt, (lh, rh) +: acc)
      case (lh +: lt, rh +: rt) if lt.contains(rh) => matchPositions(left, "*" +: right, acc)
      case (lh +: lt, rh +: rt) if rt.contains(lh) => matchPositions("*" +: left, right, acc)
      case (lh +: lt, rh +: rt) => matchPositions(lt, rt, (lh, rh) +: acc)
      case (x, y) => println(s"$x <==> $y"); Nil
    }
  }
}

class ColumnDoesNotExistException(columnName: String, header: Row) extends RuntimeException {
  override def getMessage = s"Cannot find column '$columnName' in header with columns:\n${header.data.mkString("\n")}\n"
}