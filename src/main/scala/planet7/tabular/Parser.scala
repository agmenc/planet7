package planet7.tabular

case class LineParser(delimiter: Char) {
  private val delim = s"$delimiter"
  private val quote = "\""

  // Consider: retain the previous character, instead of having all these stupid flags
  // Consider: make the internal acc a var, to reduce object creation cost. Measure performance before/after
  case class State(accumluated: Array[String], partial: String, quoting: Boolean, justDelimited: Boolean, justQuoted: Boolean) {
//    println(s"[${accumluated mkString ", "}] <= '$partial' ($quoting, $justDelimited, $justQuoted)")

    def accumulate(c: Char): State = (c, quoting) match {
      case (`delimiter`, false) if justQuoted => State(accumluated, "", false, true, false)
      case (`delimiter`, false) => State(partial +: accumluated, "", false, true, false)
      case ('\"', true) => State(partial +: accumluated, "", false, false, true)
      case ('\"', false) => State(accumluated, "", true, false, true)
      case _ => State(accumluated, partial + c, quoting, false, false)
    }

    def data = if (justQuoted) accumluated reverse else (partial +: accumluated) reverse
  }

  val startState = State(Array[String](), "", false, false, false)

  private[tabular] def read(line: String): Row = Row(line.foldLeft(startState)((state: State, next: Char) => state accumulate next).data)
  private[tabular] def write(row: Row): String = row.data.mkString(s"$delimiter")
}

// TODO - CAS - 24/11/14 - Make Parser a trait, so that users can switch in their own, using their favourite library
object Parser {
  def default: LineParser = new LineParser(',')
  def apply(delimiter: Char): LineParser = new LineParser(delimiter)
}
