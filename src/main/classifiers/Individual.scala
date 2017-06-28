/* Class representing a single sample of a dataset */
class Individual(dataString: String) {
  // Get sample class
  val target: String = dataString.split(' ').last
  // Get sample attributes without class csv
  val attributes: String = dataString.split(' ').take(dataString.split(' ').size-1).mkString(",")
  // Get whole individual with csv
  val data: String = dataString.split(' ').mkString(",")

  override def toString = data
}