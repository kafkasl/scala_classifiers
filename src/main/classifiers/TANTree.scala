import scala.collection.mutable
import scala.collection.mutable.{HashMap, MultiMap, Set}
import scala.math.log


class TANTree[T](maxWeightTree: mutable.Map[Set[String], Boolean],
                         targetToValues: HashMap[String, Set[String]] with MultiMap[String, String],
                         varToValues: HashMap[String, Set[String]] with MultiMap[String, String],
                         counts: mutable.Map[Set[String], Double],
                         classCounts: Map[String, Double],
                         classProbabilities: Map[String, Double]){


  val target = new Root(targetToValues.head._1)
  /* We choose first (arbitrarily) attr to be the root of the tree */
  val root = new Root(maxWeightTree.head._1.head)

  /* Actual tree bulding call*/
  val tree = new HashMap[String, TANNode]()
  buildTree(root, tree)


  /* Recursive methods which builds a tree with parent as root of it */
  private def buildTree(parent: TANNode, resultTree: HashMap[String, TANNode]): Unit = {

    for (elem <- maxWeightTree) {
      val key1 = elem._1.head
      val key2 = elem._1.tail.head
      if (parent.value == key1) {
        resultTree(key2) = new Node(parent, key2)
        maxWeightTree.remove(elem._1)
        buildTree(resultTree(key2), resultTree)
      }
      if (parent.value == key2) {
        resultTree(key1) = new Node(parent, key1)
        maxWeightTree.remove(elem._1)
        buildTree(resultTree(key1), resultTree)
      }
    }
  }

  /* Wrapper function which decides which kind of prediction is selected (mode) and defines the corresponding functions
* for each of the 4 possibilites */
  def predict(attr: String, predictor: Int): Array[(String, Double)] = {

    /* Actual predict method independent of the selected mode through defined input functions */
    def _predict(f: Double => Double, computeAttrProb: (Double, Double) => Double, combine: (Double, Double) => Double): Array[(String, Double)] = {

      var predictions = new Array[(String, Double)](0)
      val attrMap = new mutable.HashMap[String, String]()
      attr.split(",").foreach(u => {
        val k, v = u.split(":").head
        attrMap(k) = u
      })
      for (t <- classProbabilities.keys.toList) {

        var probability: Double = f(classProbabilities(t))
        for (field <- attr.split(",")) {
          val pV = field.split(":")
          val param = pV(0)
          val value = pV(1)
          if (root.value == param) {
            val p = computeAttrProb(counts(Set(field, t)), classCounts(t))
            probability = combine(probability, f(p))
          } else {
            val parentRV = tree(param).parent.value
            val parentVal = attrMap(parentRV)
            val p = computeAttrProb(counts(Set(parentVal, field, t)), counts(Set(parentVal, t)))
            probability = combine(probability, f(p))
          }

        }
        predictions = predictions :+ (t, probability)
      }
      predictions.sortBy(-_._2)
    }

    /* Switch choosing prediction mode
    * 0. Simple probabilities without Laplace correction
    * 1. Log probabilities without Laplace correction
    * 2. Simple probabilities with Laplace correction
    * 3. Log probabilities with Laplace correction
    * */
    predictor match {
      case 0 => _predict(x => x, (attr, t) => attr/t, (x, y) => x*y)
      case 1 => _predict(x => log(x), (attr, t) => attr/t, (x, y) => x+y)
      case 2 => _predict(x => x, _laplaceCorrection , (x, y) => x*y)
      case 3 => _predict(x => log(x), _laplaceCorrection , (x, y) => x+y)
    }  }

  private def _laplaceCorrection(attr: Double, target: Double): Double = {
    val laplaceFactor = 1
    (attr + laplaceFactor) / (target + classProbabilities.size*laplaceFactor)
  }

  /* Fancy printing of the tree */
  override def toString: String = {
    var string = "Tree target "
    string += target + "\n"
    string += "Tree root "
    string += root + "\n"
    tree.foreach(u => string += u.toString + "\n")
    string
  }

  /* Interface that all tree nodes implement (and defines their toString method) */
  trait TANNode {
    val parent: TANNode
    val value: String

    override def toString = {
      val result = if (parent == this) value else value + " -> " + this.parent.value
      result
    }
  }

  /* Represents a normal node of the tree */
  class Node(val parent: TANNode, val value: String) extends TANNode{}

  /* Represents the actual root of the tree (node is its own parent) */
  class Root(val value: String) extends TANNode{
    val parent: TANNode = this
  }
}


