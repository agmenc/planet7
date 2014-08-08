package planet7.relational2

trait RelationalDataSource {
  def header: Row
  def rows: Traversable[Row]
}