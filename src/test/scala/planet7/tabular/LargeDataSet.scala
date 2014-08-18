package planet7.tabular

object LargeDataSet {
  val largeDataFile = "large_dataset.csv"
  val expectedHeader = Row(Array("id", "first_name", "last_name", "dob", "email", "country", "ip_address", "comment", "fee paid"))
  val expectedFirstRow = Row(Array("1", "Louise", "Fernandez", "10/6/2009", "lfernandez@jaxspan.name", "Sudan", "2.165.175.158", "orci vehicula condimentum curabitur in libero ut massa volutpat convallis", "£825877.71"))
  val expectedLastRow = Row(Array("25000", "Craig", "Sullivan", "11/12/2000", "csullivan@livepath.edu", "Saint Pierre and Miquelon", "146.20.244.214", "mi sit amet lobortis sapien sapien non mi integer ac neque duis bibendum morbi non quam nec dui", "£138363.42"))
  val expectedRowCount = 25000
}