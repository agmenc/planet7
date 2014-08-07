package planet7.relational

import java.io.{BufferedReader, InputStream, InputStreamReader}

trait CsvSupport {
  // TODO - CAS - 07/08/2014 - A Y-shaped pipeline (spits out two CSVs)
  // TODO - CAS - 07/08/2014 - Aggregator 1 - combine multiple columns
  // TODO - CAS - 07/08/2014 - Aggregator 2 - combine multiple rows - provide a predicate for row grouping/inclusion/exclusion

  case class Csv(headers: Seq[String], data: Traversable[Seq[String]]) {
    def rows: Traversable[Row] = data.map(row => Row(headers zip row))
    def map(f: Row => Row): Csv = Csv(rows map f)
    def filter(p: Row => Boolean): Csv = Csv(rows filter p)

    /**
     * Combines a rename with restructureColumns
     */
    def renameAndRestructure(columnMappings: (String,String)*): Csv = map(RowTransforms.renameAndRestructure(Map(columnMappings:_*)))

    def rename(nameChanges: (String,String)*): Csv = map(RowTransforms.rename(Map(nameChanges:_*)))

    /**
     * Change the column order. Any newly-introduced columns will be empty. Any columns not defined in newColumnOrder will be removed 
     */
    def restructure(newColumnOrder: String*): Csv = Csv(rows map (_.restructure(newColumnOrder:_*)))

    /**
     * Transform existing column values to new values, as defined by your function.
     */
    def remap(mappings: (String, (String) => String)*): Csv = Csv(rows map (_.remap(Map(mappings:_*))))
    
    def toTruncString = (headers.mkString(",") + "\n") + contentsToShow
    private def contentsToShow = if (rows.size > 2) (rows take 3 mkString "\n") + "\n..." else rows mkString "\n"

    override def toString: String = headers.mkString(",") + "\n" + rows.map(_.toString).mkString("\n") + "\n"
  }

  /**
   * Allow client to simply name a field, instead of showing a rename tuple, if the field name isn't changing:
   *
   * Example:
   *    {{{
   *      Csv(readFile("large_dataset.csv"))
   *        .renameAndRestructure(
   *          "first_name" -> "First Name",
   *          "last_name",    // No tuple required
   *          "fee paid" -> "Fee Paid")
   *        .remap("last_name" -> (_.toUpperCase))
   *    }}}
   *
   */
  implicit def StringToTuple(s: String): (String, String) = s -> s

  object Csv {
    def apply(inputStream: InputStream): Csv = Csv(DefaultRelationalDatasources.PimpFromInputStream(inputStream))
    def apply(rawData: String): Csv = Csv(DefaultRelationalDatasources.PimpFromString(rawData))
    def apply(external: RelationalDataSource): Csv = Csv(external.headers, external.data)
    def apply(rows: Traversable[Row]): Csv = Csv(rows.head.columnNames, rows.map(_.columnValues))
    def apply(csv: Csv, csvs: Csv*): Csv = Csv((csv +: csvs).head.headers, (csv +: csvs).flatMap(_.data)(collection.breakOut): Seq[Seq[String]])
  }

  object DefaultRelationalDatasources {
    // TODO - CAS - 07/08/2014 - PimpFromFile

    implicit class PimpFromString(rawData: String) extends RelationalDataSource {
      private val allRows = rawData.trim.split("\n")
      override def headers = toRowValues(allRows.head)
      override def data = allRows.tail filter(_.trim.nonEmpty) map toRowValues
    }

    private def toRowValues(s: String) = s.split(",").toSeq

    implicit class PimpFromInputStream(is: InputStream) extends RelationalDataSource {
      private val br: BufferedReader = new BufferedReader(new InputStreamReader(is))
      override def headers = toRowValues(br.readLine())
      override def data = new Traversable[Seq[String]] {
        override def foreach[U](f: (Seq[String]) => U) =  {
          var  line = br.readLine()
          while(line != null) {
            f(toRowValues(line))
            line = br.readLine()
          }
        }
      }
    }
  }

  trait RelationalDataSource {
    def headers: Seq[String]
    def data: Traversable[Seq[String]]
  }
}