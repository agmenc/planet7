package planet7.tree

import org.scalatest.WordSpec

class TreeDiffSpec extends WordSpec {
  "Comparing two JSON trees results in a tuple of two smaller diff trees" in {
    val left =
      """
        |{
        | "blaaaahh": "blah blah blah",
        | "address": {
        |   "street": "Main Street",
        |   "postcode": "ABC123"
        | },
        | "name": "bob",
        | "blah": "more blah"
        |}
      """.stripMargin

    val right =
      """
        |{
        | "blaaaahh": "blah blah blah",
        | "address": {
        |   "postcode": "ABC123"
        | },
        | "name": "fred",
        | "DOB": 1987,
        | "blah": "more blah"
        |}
      """.stripMargin

    val expected = (
      """
        |{
        | "address": {
        |   "postcode": "ABC123"
        | },
        | "name": "bob"
        |}
      """.stripMargin,
      """
        |{
        | "name": "fred",
        | "DOB": 1987,
        |}
      """.stripMargin
      )

    // TODO - CAS - 07/08/2014 - Er, write the assertions
    // ADDED   = the diffs, filtered by "left is not on right"
    // REMOVED = the diffs, filtered by "right is not on left"
    // CHANGED = left != right

    fail("Write me")
  }
}