package planet7.relational

import java.io.{BufferedReader, InputStream, InputStreamReader}

trait CsvSupport {
  case class Csv(headers: Seq[String], data: Iterator[Seq[String]]) {
    def rows: Iterator[Row] = data.map(row => Row(headers zip row))

    private val headerRow = Row(headers zip headers)
    private def andHeader(it: Iterator[Row]): Iterator[Row] = Iterator.single[Row](headerRow) ++ it

    def map(f: Row => Row): Csv = Csv(andHeader(rows) map f)
    def filter(p: Row => Boolean): Csv = Csv(andHeader(rows filter p))

    /**
     * Combines a rename with restructureColumns
     */
    def renameAndRestructure(columnMappings: (String,String)*): Csv = map(RowTransforms.renameAndRestructure(columnMappings))

    def rename(nameChanges: (String,String)*): Csv = map(RowTransforms.rename(Map(nameChanges:_*)))

    /**
     * Change the column order. Any newly-introduced columns will be empty. Any columns not defined in newColumnOrder will be removed 
     */
    def restructure(newColumnOrder: String*): Csv = Csv(andHeader(rows) map (_.restructure(newColumnOrder: _*)))

    /**
     * Transform existing column values to new values, as defined by your function.
     */
    def remap(mappings: (String, (String) => String)*): Csv = Csv(andHeader(rows map (_.remap(Map(mappings:_*)))))
    
    def toTruncString = (headers.mkString(",") + "\n") + contentsToShow
    private def contentsToShow = rows take 3 mkString "\n"

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
    def apply(rowsIncludingHeader: Iterator[Row]): Csv = new Csv(rowsIncludingHeader.next.columnNames, rowsIncludingHeader.map(_.columnValues))
    def apply(csv: Csv, csvs: Csv*): Csv = Csv(csv.headers, (csv +: csvs).flatMap(_.data)(collection.breakOut): Iterator[Seq[String]])
  }

  object DefaultRelationalDatasources {
    // TODO - CAS - 07/08/2014 - PimpFromFile

    implicit class PimpFromString(rawData: String) extends RelationalDataSource {
      private val allRows = rawData.trim.split("\n")
      override def headers = toRowValues(allRows.head)
      override def data = allRows.tail.iterator.filter(_.trim.nonEmpty) map toRowValues
    }

    private def toRowValues(s: String) = s.split(",").toSeq

    implicit class PimpFromInputStream(is: InputStream) extends RelationalDataSource {
      private val br: BufferedReader = new BufferedReader(new InputStreamReader(is))
      override def headers = toRowValues(br.readLine())
      override def data = new Iterator[Seq[String]] {
        private var line = br.readLine()

        override def hasNext = line != null

        override def next() = {
          val oldLine = line
          line = br.readLine()
          if (line == null) is.close()
          toRowValues(oldLine)
        }
      }
    }
  }

  trait RelationalDataSource {
    def headers: Seq[String]
    def data: Iterator[Seq[String]]
  }
}