package planet7.relational

import org.scalatest.WordSpec
import planet7.Diff

class CaseClassSpec extends WordSpec {

  "We can diff collections containing case class instances" in {
    case class SomeCaseClass(name: String, age: Integer, role: String)

    object Empty extends SomeCaseClass("", 0 , "")

    case object SccDiffer extends Differentiator[SomeCaseClass] {
      def zero = Empty
      def key(u: SomeCaseClass) = u.name
    }

    val left: List[SomeCaseClass] = List(
      SomeCaseClass("a", 2, "x"),
      SomeCaseClass("b", 3, "p"),
      SomeCaseClass("c", 4, "r")
    )

    val right: List[SomeCaseClass] = List(
      SomeCaseClass("a", 2, "x"),
      SomeCaseClass("b", 3, "q"),
      SomeCaseClass("d", 5, "s")
    )

    val result: Seq[(SomeCaseClass, SomeCaseClass)] = Diff(left, right, SccDiffer)

    assert(result === List(
      SccDiffer.zero -> SomeCaseClass("d", 5, "s"),
      SomeCaseClass("c", 4, "r") -> SccDiffer.zero,
      SomeCaseClass("b", 3, "p") -> SomeCaseClass("b", 3, "q")
    ))
  }

  "We can diff one case class instance with another" in {
    case class SomeOtherCaseClass(name: String, id: Integer, address: String)

    implicit class FieldSucker(socc: SomeOtherCaseClass) {
      def fields: List[Field] = List("name" -> socc.name, "id" -> socc.id.toString, "address" -> socc.address)
    }

    val left = SomeOtherCaseClass("bob", 1, "abc")
    val right = SomeOtherCaseClass("bob", 1, "def")
    
    val result: Seq[(Field, Field)] = Diff(left.fields, right.fields, FieldDiffer)
    
    assert(result === List(("address", "abc") ->("address", "def")))
  }
}