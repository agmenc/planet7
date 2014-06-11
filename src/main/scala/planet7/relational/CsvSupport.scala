package planet7.relational

trait CsvSupport {
  case class Csv(headers: List[String], data: List[List[String]]) {
    def rows: List[Row] = data map(headers zip _) map Row

    /**
     * These columns will be:
     *   * renamed
     *   * retained in the output; all others will be dropped
     *   * added if they are missing
     *   * re-ordered to match the provided Sequence
     */
    def defineOutputColumns(columnMappings: (String,String)*): Csv = rename(columnMappings:_*).retainReorderOrAdd(columnMappings.map(_._2):_*)

    /**
     * These columns will be:
     *   * retained in the output; all others will be dropped
     *   * added if they are missing
     *   * re-ordered to match the provided Sequence
     */
    def retainReorderOrAdd(columns: String*): Csv = retain(columns:_*).reorderOrAdd(columns:_*)

    def rename(nameChanges: (String,String)*): Csv = renameColumns(Map(nameChanges:_*))

    /**
     * These columns will be retained in the output; all others will be dropped
     */
    def retain(columns: String*): Csv = Csv(rows map (_.retainColumns(columns:_*)))

    /**
     * These columns will be:
     *   * added if they are missing
     *   * re-ordered to match the provided Sequence
     */
    def reorderOrAdd(columns: String*): Csv = Csv(rows map (_.reorderColumnsOrAdd(columns:_*)))

    /**
     * Map column values as defined by your function
     */
    def remap(mappings: (String, (String) => String)*): Csv = Csv(rows map (_.replace(Map(mappings:_*))))
    
    private def renameColumns(deltas: Map[String,String]): Csv = Csv(headers map (renameHeader(deltas, _)), data)
    private def renameHeader(deltas: Map[String,String], header: String) = if (deltas.contains(header)) deltas(header) else header

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