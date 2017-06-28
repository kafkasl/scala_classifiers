import scala.math.pow

class ConfusionMatrix(targets: List[String]) {

  // Var holding the mapping from attributes name to confusion matrix index
  val indexMapping: Map[String, Int] = targets.view.zipWithIndex.toMap

  // Actual confusion matrix
  val matrix = Array.ofDim[Double](indexMapping.size, indexMapping.size)

  // Cached values to avoid traversing the matrix each time we need the precision
  var correctPredicted: Double = 0
  var totalPredictions: Double = 0

  /* Save a new prediction and its correct label */
  def save(prediction: String, truth: String) = {
    totalPredictions += 1
    if (prediction == truth) correctPredicted += 1
    matrix(indexMapping(prediction))(indexMapping(truth)) += 1
  }

  /* Method used to get the precision without printing all the information */
  def getAccuracy(): Double = {
    correctPredicted / totalPredictions // return accuracy
  }

  override def toString: String = {

    var string = "\nCONFUSION MATRIX\nPredicted \t Truth \n"
    for(i <- 0 until pow(matrix.length,2).toInt) {
      string += targets(i / matrix.length) + " - " + targets(i % matrix.length) + ": " + matrix(i / matrix.length)(i % matrix.length).toInt + "\n"
    }

    string += "Accuracy: " + (correctPredicted / totalPredictions) + "\n"

    var jointSensitivity: Double =     .0
    for( i <- matrix.indices){
      string += ("Sensitivity (" + targets(i) + "): ")
      var trueLabeled: Double = 0
      for (j <- matrix.indices){
        trueLabeled += matrix(j)(i)
      }
      string += matrix(i)(i)/trueLabeled + "\n"
      jointSensitivity += matrix(i)(i)/trueLabeled
    }
    string += "Mean Sensitivity: " + jointSensitivity/matrix.length + "\n"

    var jointPrecision: Double =     .0
    for( i <- matrix.indices){
      string += "Precision (" + targets(i) + "): "
      var truePredicted: Double = 0
      for (j <- matrix.indices){
        truePredicted += matrix(i)(j)
      }
      string += matrix(i)(i)/truePredicted + "\n"
      jointPrecision += matrix(i)(i)/truePredicted
    }
    string += "Mean Precision: " + jointPrecision/matrix.length + "\n"

    string // return string
  }
}