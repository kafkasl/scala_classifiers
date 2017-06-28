import scala.collection.mutable
import scala.math.log

class NaiveBayes(data: Array[Individual]) extends Classifier {

  /* Variables holding the class probabilites and counts (required for laplace correction) */
  val classProbabilities = new mutable.HashMap[String, Double]().withDefaultValue(0)

  val classCounts = new mutable.HashMap[String, Int]().withDefaultValue(0)
  val condCounts = new mutable.HashMap[String, Int]().withDefaultValue(0)

  /* Method exposing the classes of the problem (used for confusion Matrix */
  def targets =  classProbabilities.keys.toList

  train()

  /* Method responsible of counting and then normalizing the values
   * It contains to closure functions: count & normalize
   * Only class probabilities are computed because of Laplace correction happens at prediction time */
  private def train() = {

    def count(index: Int): Unit = {
      if (index < data.length ) {
        val element = data(index)
        classCounts(element.target) += 1
        for (attr <- element.attributes.split(",")) {
          condCounts(attr + "," + element.target) += 1
        }
        count(index + 1)
      }

    }

    def normalize() = {
      for ((k,v) <- classCounts)
        classProbabilities(k) = v / data.size.toDouble

    }

    /* Actual invocation of count and normalize methods inside train() */
    count(0)
    normalize()

  }

  /* Wrapper function which decides which kind of prediction is selected (mode) and defines the corresponding functions
  * for each of the 4 possibilites */
  def predict(attr: String, predictor: Int): Array[(String, Double)] = {

    /* Actual predict method independent of the selected mode through defined input functions */
    def _predict(f: Double => Double, computeAttrProb: (Int, Int) => Double, combine: (Double, Double) => Double): Array[(String, Double)] = {
      var predictions = new Array[(String, Double)](0)
      for (t <- classProbabilities.keys.toList) {
        var probability: Double = f(classProbabilities(t))
        for (field <- attr.split(",")) {
          val attrProb = computeAttrProb(condCounts(field + "," + t), classCounts(t))
          probability = combine(probability, f(attrProb))
        }
        predictions = predictions :+ (t, probability)
      }
      predictions.sortBy(-_._2)
    }

    def laplaceCorrection(attr: Int, target: Int): Double = {
      val laplaceFactor = 1
      (attr + laplaceFactor) / (target.toDouble + classProbabilities.size*laplaceFactor)
    }

    /* Switch choosing prediction mode
    * 0. Simple probabilities without Laplace correction
    * 1. Log probabilities without Laplace correction
    * 2. Simple probabilities with Laplace correction
    * 3. Log probabilities with Laplace correction
    * */
    predictor match {
      case 0 => _predict(x => x, (attr, t) => attr/t.toDouble, (x, y) => x*y)
      case 1 => _predict(x => log(x), (attr, t) => attr/t.toDouble, (x, y) => x+y)
      case 2 => _predict(x => x, laplaceCorrection , (x, y) => x*y)
      case 3 => _predict(x => log(x), laplaceCorrection , (x, y) => x+y)
    }


  }
}