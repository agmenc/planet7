package planet7.relational

trait CsvSupport {
  case class Csv(headers: List[String], data: List[List[String]]) {
    def rows: List[Row] = data map(headers zip _) map Row

    def map(f: Row => Row): Csv = Csv(rows map f)

    /**
     * Combines a rename with restructureColumns
     */
    def renameAndRestructure(columnMappings: (String,String)*): Csv = rename(columnMappings:_*).restructure(columnMappings.map(_._2):_*)

    def rename(nameChanges: (String,String)*): Csv = map(RowTransforms.rename(nameChanges:_*))

    /**
     * Change the column order. Any newly-introduced columns will be empty. Any columns not defined in newColumnOrder will be removed 
     */
    def restructure(newColumnOrder: String*): Csv = Csv(rows map (_.restructure(newColumnOrder:_*)))

    /**
     * Transform existing column values to new values, as defined by your function.
     */
    def remap(mappings: (String, (String) => String)*): Csv = Csv(rows map (_.replace(Map(mappings:_*))))
    
    def toTruncString = (headers.mkString(",") + "\n") + contentsToShow
    private def contentsToShow = if (rows.size > 2) (rows take 3 mkString "\n") + "\n..." else rows mkString "\n"

    override def toString: String = (headers.mkString(",") :: rows.map(_.toString)).mkString("\n") + "\n"
  }

  object Csv {
    def apply(rawData: String): Csv = Csv(DefaultRelationalDatasources.PimpFromString(rawData))
    def apply(external: RelationalDataSource): Csv = Csv(external.headers, external.data)
    def apply(rows: List[Row]): Csv = Csv(rows.head.columnNames, rows.map(_.columnValues))
    def apply(csvs: Csv*): Csv = apply(csvs)
    def apply(csvs: Iterable[Csv]): Csv = Csv(csvs.head.headers, csvs.flatMap(_.data)(collection.breakOut): List[List[String]])
  }

  object DefaultRelationalDatasources {
    implicit class PimpFromString(rawData: String) extends RelationalDataSource {
      val allRows = rawData.trim.split("\n").toList
      override def headers = toRowValues(allRows.head)
      override def data = allRows.tail filter(_.trim.nonEmpty) map toRowValues

      private def toRowValues(s: String) = s.split(",").toList
    }

    // TODO - CAS - 11/06/2014 - Buffered sources, common CSV parsers from pull requests, etc
  }

  trait RelationalDataSource {
    def headers: List[String]
    def data: List[List[String]]
  }
}