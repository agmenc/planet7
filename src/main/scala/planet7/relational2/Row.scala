package planet7.relational2

/**
 * A Row is an Array of data
 */
case class Row(data: Array[String]) {
  override def toString = data.mkString(",")
}