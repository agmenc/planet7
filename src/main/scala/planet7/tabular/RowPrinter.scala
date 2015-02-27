package planet7.tabular

class RowPrinter() {
  def showDiffs(left: Row, right: Row): String = matchPositions(left, right) match {
    case (l, r) => s"""
                      |$l
                      |$r
                      |""".stripMargin
  }

  def matchPositions(left: Row, right: Row): (Row, Row) =
    matchPositions(left.data.toSeq, right.data.toSeq, Nil)
      .map(addBrackets _ andThen addPadding)
      .reverse.unzip match {
      case (l, r) => (Row(l.toArray), Row(r.toArray))
    }

  def addBrackets(t: (String, String)): (String, String) = (t._1, t._2) match {
    case (l, r) if l == r => (l, r)
    case ("*", r) => ("", s"[$r]")
    case (l, "*") => (s"[$l]", "")
    case (l, r) => (s"[$l]", s"[$r]")
  }

  def addPadding(t: (String, String)): (String, String) = (t._1, t._2) match {
    case (l, r) => pad(l, r, Math.max(l.length, r.length))
  }

  def pad(left: String, right: String, length: Int): (String, String) =
    (left.padTo(length, ' '), right.padTo(length, ' '))

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