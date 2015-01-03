package planet7.tabular

case class Row(data: Array[String], validationFailures: Seq[String] = Nil) {
  override def toString = data.mkString(",")

  def isInvalid = validationFailures != Nil && validationFailures.nonEmpty

  override def canEqual(that: Any) = that.isInstanceOf[Row]

  override def equals(that: Any) = that match {
    case thatRow: Row => this.data.deep == thatRow.data.deep
    case _ => false
  }
}