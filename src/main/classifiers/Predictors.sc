import scala.io.Source

object test {
  def testClassifier(clfName: String, dataPath: String, mode: Int, cvFolds: Int) = {

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
//    val clf = clfName match {
//      case "MaxAPosteriori" => new MaxAPosteriori(dataArray)
//      case "NaiveBayes" => new NaiveBayes(dataArray)
//      case "TANaiveBayes" => new TANaiveBayes(dataArray)
//    }
//    val confusionMatrix = new ConfusionMatrix(clf.targets)

    val clf = new TANaiveBayes(dataArray)
    println("TAN Tree")
    clf.tree.toString
//    for( i <- 0 until 10) {
//      val data = scala.util.Random.shuffle(dataArray.toList).toArray
//
//
//      println("Clf => " + clfName + " with " + cvFolds + "-fold cross-validation.\n")
//      for (fold <- 0 until cvFolds) {
//
//
//        val foldSize = n / cvFolds
//        val trainSet = data.slice(0, fold * foldSize) ++ data.slice((fold + 1) * foldSize, n)
//        val testSet = data.slice(fold * foldSize, (fold + 1) * foldSize)
//


        //        val clf = clfName match {
//          case "MaxAPosteriori" => new MaxAPosteriori(trainSet)
//          case "NaiveBayes" => new NaiveBayes(trainSet)
//          case "TANaiveBayes" => new TANaiveBayes(trainSet)
//        }



//        for (element <- testSet) {
//          val predictions = clf.predict(element.attributes, mode)
//          confusionMatrix.save(predictions(0)._1, element.target)
//        }
//      }
//    }
//    println(confusionMatrix.toString)

  }

  val path = "/home/hydra/miri/adm/labs/assignment.1/src/main/resources/votesTr.txt"
  testClassifier("TANaiveBayes", path, 3, 10)

}
