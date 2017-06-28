import scala.io.Source

trait Classifier {
  def targets: List[String]
  def predict(attr: String, mode: Int): Array[(String, Double)]
}

object TestsExecutor extends App
{

  def testClassifier(clfName: String, dataPath: String, mode: Int, cvFolds: Int, test: Boolean) = {

    def readDataSet(path: String): Array[Individual] = {
      val dataset: List[String] = Source.fromFile(path).getLines.toList
      var data: Array[Individual] = new Array[Individual](0)

      for (line <- dataset.slice(1, dataset.size)) {
        var individual = new Individual(line)
        data = data :+ individual
      }
      data
    }

    val dataArray = readDataSet(dataPath)
    val n = dataArray.length
    val data = scala.util.Random.shuffle(dataArray.toList).toArray

    val _clf = clfName match {
      case "MaxAPosteriori" => new MaxAPosteriori(data)
      case "NaiveBayes" => new NaiveBayes(data)
      case "TANaiveBayes" => new TANaiveBayes(data)
    }

    print("\nClf => " + clfName)
    if(test) println(" with " + cvFolds + "-fold cross-validation.") else println(" without cross-validation.")
    var accAccuracy = 0.0
    var first = true
    val iterations = 10
    // Are we testing with 10x10CV or not?
    if (test) {
      // iterations or 10x10CV
      for (i <- 0 until iterations) {
        // cvFolds of CVC
        for (fold <- 0 until cvFolds) {
          val confusionMatrix = new ConfusionMatrix(_clf.targets)


          // Partition the dataset
          val foldSize = n / cvFolds
          val trainSet = data.slice(0, fold * foldSize) ++ data.slice((fold + 1) * foldSize, n)
          val testSet = data.slice(fold * foldSize, (fold + 1) * foldSize)


          // Choose classifier to be used and train it (done in the constructor)
          val clf = clfName match {
            case "MaxAPosteriori" => new MaxAPosteriori(trainSet)
            case "NaiveBayes" => new NaiveBayes(trainSet)
            case "TANaiveBayes" => new TANaiveBayes(trainSet)
          }


          // Validate classifier
          for (element <- testSet) {
            val predictions = clf.predict(element.attributes, mode)
            confusionMatrix.save(predictions(0)._1, element.target)
          }
          // Aggregate accuracy
          accAccuracy += confusionMatrix.getAccuracy()

          // Print ConfMatrix just in first iteration for reference
          if (first) {
            first = false
            println(confusionMatrix.toString)
          }
        }
      }
      val totalAccuracy = accAccuracy / (cvFolds*iterations)
      println("Accuracy: " + totalAccuracy)
    } else {
      // Traing and test with whole dataset and print confusion matrix
      val confusionMatrix = new ConfusionMatrix(_clf.targets)
      for (element <- data) {
        val predictions = _clf.predict(element.attributes, mode)
        confusionMatrix.save(predictions(0)._1, element.target)
      }
      println(confusionMatrix)
    }

  }


  var datasetPath = List("cmcTr.txt","germanTr.txt","mushroomTr.txt","pimaTr.txt","titanicTr.txt","votesTr.txt")
  val basePath = "../resources/"

  // Test classifiers with 10x10CV
  var crossValidation = true
  for(filePath <- datasetPath){
    println("Testing dataset: " + filePath)
    testClassifier("MaxAPosteriori", basePath + filePath, 0, 10, crossValidation)
    testClassifier("NaiveBayes", basePath + filePath, 3, 10, crossValidation)
    testClassifier("TANaiveBayes", basePath + filePath, 3, 10, crossValidation)
  }

  // Test and train classifiers with whole dataset
  crossValidation = false
  datasetPath = List("cmcTr.txt","germanTr.txt","lensesTr.txt","mushroomTr.txt","pimaTr.txt","titanicTr.txt","votesTr.txt","weatherNominalTr.txt")
  for(filePath <- datasetPath){
    println("Testing dataset: " + filePath)
    testClassifier("MaxAPosteriori", basePath + filePath, 0, 1, crossValidation)
    testClassifier("NaiveBayes", basePath + filePath, 3, 1, crossValidation)
    testClassifier("TANaiveBayes", basePath + filePath, 3, 1, crossValidation)
  }
}