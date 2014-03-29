package planet7.relational

import org.scalatest.WordSpec
import planet7.relational.csv.FieldSupport._
import scala.Some
import planet7.Diff

class CaseClassSpec extends WordSpec {

//  implicit class SomeCaseClassDiffable(cc: SomeCaseClass) extends Diffable[Field] {
//    def zero = EmptyField
//
//    def keyExtractor(maybeU: Option[Field]) = ???
//
//    def sorted(key: (Option[Field]) => String) = List("name" -> cc.name, "age" -> cc.age.toString, "role" -> cc.role)
//  }

  "We can diff one case class instance with another" in {
//    assert(result === List(("role", "x") -> ("role", "y")))
  }

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

    val result: List[(SomeCaseClass, SomeCaseClass)] = Diff(left, right, SccDiffer)

    assert(result === List(
      SccDiffer.zero -> SomeCaseClass("d", 5, "s"),
      SomeCaseClass("c", 4, "r") -> SccDiffer.zero,
      SomeCaseClass("b", 3, "p") -> SomeCaseClass("b", 3, "q")
    ))
  }
}