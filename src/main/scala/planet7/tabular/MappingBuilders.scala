package planet7.tabular

import scala.annotation.implicitNotFound

trait MappingBuilders {
  implicit def toRowTransformer(mapping: (String, String => Row => Row => Row)): Row => Row => Row =
    mapping match {
      case (colName, mapper) => mapper(colName)
    }
  
  object X {
    implicit def toBigDecimal(s: String): BigDecimal = BigDecimal(s)
  }

  // Trait/classes instead of lambda, to avoid implicit conflicts. Also allows better error messages.
  @implicitNotFound(msg = "Implicit parameter missing. Please provide an implicit instance of Conv[${T}].")
  case class Conv[T](f: String => T) {
    def apply(s: String): T = f(s)
  }

  implicit val toBD = Conv(s => BigDecimal(s))
  implicit val toInt = Conv(_.toInt)

  // TODO - CAS - 05/03/15 - Shapeless, so I don't have to have 22 arity-specific methods, or use macros directly
  def given[T1](col1: String) = ???

  def given[T1, T2]
    (col1: String, col2: String)
    (pf: PartialFunction[(T1, T2), String])
    (implicit t1Conv: Conv[T1], t2Conv: Conv[T2]): String => Row => Row => Row = {
        
    (targetColumn: String) => (header: Row) => {
      val col1index = header.data.indexOf(col1)
      val col2index = header.data.indexOf(col2)
      val targetIndex = header.data.indexOf(targetColumn)

      row: Row => {
        val data1: T1 = t1Conv(row.data(col1index))
        val data2: T2 = t2Conv(row.data(col2index))
        val result: String = pf(data1, data2)
        val newRow: Row = row.copy()
        newRow.data(targetIndex) = result
        newRow
      }
    }
  }
  def given[T1,T2,T3](colMod1: String, colMod2: String, colMod3: String) = ???
}