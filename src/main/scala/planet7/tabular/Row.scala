package planet7.tabular

/**
 * A Row is an Array of data
 */
case class Row(data: Array[String]) {
  override def toString = data.mkString(",")

  override def canEqual(that: Any) = that.isInstanceOf[Row]

  override def equals(that: Any) = that match {
    case thatRow: Row => this.data.deep == thatRow.data.deep
    case _ => false
  }
}