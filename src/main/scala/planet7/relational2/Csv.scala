package planet7.relational2

/**
 * A Csv is a Traversable input source and a set of transformations
 * A Csv provides a Traversable of ???
 *
 * All materialisation of the datasource is done OUTSIDE of the Csv.
 */
case class Csv(source: RelationalDataSource) {
  def header = source.header
  // TODO - CAS - 08/08/2014 - Use withFilter on the Traversable[Row], as filter materialises the list when it filters it
}

//object Csv {
//  def apply[A](x: A)(implicit f: A => RelationalDataSource): Csv = Csv(f(x))
//}