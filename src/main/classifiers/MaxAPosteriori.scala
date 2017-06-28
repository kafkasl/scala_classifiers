import scala.collection.mutable


class MaxAPosteriori(data: Array[Individual]) extends Classifier{

  /* Variables holding the conditional and class probabilites */
  val condProbabilities = new mutable.HashMap[String, Double]().withDefaultValue(0)
  val classProbabilities = new mutable.HashMap[String, Double]().withDefaultValue(0)

  /* Method exposing the classes of the problem (used for confusion Matrix */
  def targets: List[String] =  classProbabilities.keys.toList

  /* When class is created (we are inside constructor) we call the training method */
  train()

  /* Method responsible of counting and then normalizing the values
   * It contains to closure functions: count & normalize */
  private def train() = {

    val classCounts = new mutable.HashMap[String, Int]().withDefaultValue(0)
    val attrCounts = new mutable.HashMap[String, Int]().withDefaultValue(0)
    val condCounts = new mutable.HashMap[String, Int]().withDefaultValue(0)

    def count(index: Int): Unit = {
      if (index < data.length ) {
        val element = data(index)
        classCounts(element.target) += 1
        attrCounts(element.attributes) += 1
        condCounts(element.data) += 1
        count(index + 1)
      }
    }

    def normalize() = {
      for ((k,v) <- attrCounts) {
        for ((k2, v2) <- classCounts) {
          condProbabilities(k + "," + k2) = condCounts(k + "," + k2) / attrCounts(k).toDouble
        }
      }
      for ((k,v) <- classCounts) {
        classProbabilities(k) = classCounts(k) / data.size.toDouble
      }
    }
    count(0)
    normalize()
  }

  /* Function used to make predictions given a sample (attr). Mode parameter is only used in NB and TANB */
  def predict(attr: String, mode: Int): Array[(String, Double)] = {
    var predictions = new Array[(String, Double)](0)
    var highest: Double = 0d
    for ((k, v) <- classProbabilities) {
      val conditionedProbability = condProbabilities(attr + "," + k)
      if (conditionedProbability > highest) predictions = Array.empty[(String, Double)]
      if (conditionedProbability >= highest) {
        highest = conditionedProbability
        predictions = predictions :+ (k, highest)
      }
    }
    predictions
  }
}