import scala.collection.mutable
import scala.collection.mutable.{HashMap, MultiMap, Set}
import scala.math.log
import scala.util.Sorting


class TANaiveBayes(data: Array[Individual]) extends Classifier {

  /* Target probabilities */
  val classProbabilities = new mutable.HashMap[String, Double]().withDefaultValue(0)

  /* Class counts for quick acces */
  val classCounts: mutable.Map[String, Double] = new mutable.HashMap[String, Double]().withDefaultValue(0)
  /* Contains all the conditioned counts (an attr given the target and two attrs given a target)
   * We use as key of the Map a Set (with 1 or 2 attributes and the target) */
  val counts: mutable.Map[Set[String], Double] = new mutable.HashMap[Set[String], Double]().withDefaultValue(0)

  /* Method exposing the classes of the problem (used for confusion Matrix */
  def targets =  classProbabilities.keys.toList

  /* All computations required to build the TAN tree are encapsulated inside train method */
  val tree: TANTree[String] = train()

  private def _laplaceCorrection(attr: Double, target: Double): Double = {
    val laplaceFactor = 1
    (attr + laplaceFactor) / (target + classProbabilities.size*laplaceFactor)
  }

  /* Method responsible of counting and then normalizing the values
   * It contains to closure functions: count, normalize, individualInformation, conditionalInformation, and buildMaxWeightSpanTree
   * Only class probabilities are computed because of Laplace correction happens at prediction time */
  private def train() = {
    val varsToValues = new HashMap[String, Set[String]] with MultiMap[String, String]
    val targetToValues = new HashMap[String, Set[String]] with MultiMap[String, String]

    /* Count all conditioned appearances and save all the possible values of each attribute and target */
    def count(index: Int): Unit = {
      if (index < data.length ) {
        val element = data(index)
        classCounts(element.target) += 1
        val (key, value) = (element.target.split(":")(0), element.target.split(":")(1))
        if (!targetToValues.entryExists(key, _ == value)) targetToValues.addBinding(key, value)
        val attrs = element.attributes.split(",")
        for (i <- 0 until attrs.length) {
          var j = i + 1
          counts(Set(attrs(i), element.target)) += 1
          val (key, value) = (attrs(i).split(":")(0), attrs(i).split(":")(1))
          if (!varsToValues.entryExists(key, _ == value)) varsToValues.addBinding(key, value)
          while (j <= attrs.length-1) {
            counts(Set(attrs(i), attrs(j), element.target)) += 1
            j += 1
          }
        }
        count(index + 1) // recursive call
      }

    }

    def normalize() = {
      for ((k,v) <- classCounts)
        classProbabilities(k) = v / data.size.toDouble
    }

    /* Conditional mutual information of attribute labelX and labelY given the target class*/
    def individualInformation(labelX: String, setX: Set[String],
                              labelY: String, setY: Set[String],
                              labelZ: String, setZ: Set[String]): Double = {
      var acc =   .0
      for(x <- setX) {
        for(y <- setY) {
          for(z <- setZ) {
            val (xAttr, yAttr, zTarget) = (labelX + ":" + x, labelY + ":" + y, labelZ + ":" + z)
            val pxyz: Double = _laplaceCorrection(counts(Set(xAttr, yAttr, zTarget)), data.length)
            val pxy_z: Double = _laplaceCorrection(counts(Set(xAttr, yAttr, zTarget)), classCounts(zTarget))
            val px_z: Double = _laplaceCorrection(counts(Set(xAttr, zTarget)), classCounts(zTarget))
            val py_z: Double = _laplaceCorrection(counts(Set(yAttr, zTarget)), classCounts(zTarget))
            acc +=  (pxyz * log(pxy_z / (px_z * py_z)) )
          }
        }
      }
      acc // return acc
    }

    /* Computation of all conditional mutual information among all different attributes */
    def conditionalMutualInformation() = {
      val vars = varsToValues.toVector
      val targets = targetToValues.toVector

      // Variable containing all the conditional mutual information for each 2 attrs
      val ipXY_Z = new mutable.HashMap[Set[String], Double]()

      var x: Integer = 0
      while( x < vars.size) {
        var y: Integer = x + 1
        while( y < vars.size) {
          val condMutualInformation = individualInformation(vars(x)._1, vars(x)._2, vars(y)._1, vars(y)._2, targets(0)._1, targets(0)._2)
          ipXY_Z(Set(vars(x)._1, vars(y)._1)) = condMutualInformation
          y += 1
        }
        x += 1
      }
      ipXY_Z // return ipXY_Z
    }

    /* Method thas build the max spanning tree, it is divided into two simpler functions: edgeList and kruskal */
    def buildMaxWeightSpanTree(weights: mutable.HashMap[Set[String], Double] ): mutable.Map[Set[String], Boolean] = {
      val n: Integer = varsToValues.size
      val A = new mutable.HashMap[Set[String], Boolean]().withDefaultValue(false)
      val F = new mutable.HashMap[String, UnionFindSet[String]]()
      varsToValues.keys.foreach( (attr) => F(attr) = UnionFindSet.makeSet(attr) )

      /* Auxiliary function building a convenient data structure (a List) with all the edges sorted, ready to be used by Kruskal algorithm */
      def edgeList(weights: mutable.HashMap[Set[String], Double], list: List[(String, String, Double)]): List[(String, String, Double)] = {
        if (weights.nonEmpty) {
            val (edges, weight) = weights.head
            edgeList(weights.tail, list ++ List( (edges.toList(0), edges.toList(1), weight) ))

        } else {
          Sorting.stableSort(list, (x: (String,String,Double),y: (String,String,Double)) => x._3 > y._3).toList
        }
      }

      /* Kruskal algorithm that builds the mst */
      def kruskal(edges: List[(String, String, Double)]): Unit = {
        if (edges.nonEmpty) {
          val (i, j, w) = edges.head
          if (UnionFindSet.find(F(i)) != UnionFindSet.find(F(j))) {
            UnionFindSet.union(F(i), F(j))
            A(Set(i,j)) = true
          }
          kruskal(edges.tail)
        }
      }

      // Create an ordered list of the mutual information
      val edges = edgeList(weights, List())

      // Build Max Spanning Tree with Kruskal method
      kruskal(edges)
      A // return A
    }


    /* Actual invocation of all the methods inside train() */
    count(0)
    normalize()

    // Compute conditional mutual information
    val weights: mutable.HashMap[Set[String], Double] = conditionalMutualInformation()

    // Build Max Spanning Tree
    val maxWeightTree: mutable.Map[Set[String], Boolean] = buildMaxWeightSpanTree(weights)

    // Direct tree from root
    val tree: TANTree[String] = new TANTree[String](maxWeightTree, targetToValues, varsToValues, counts, classCounts.toMap, classProbabilities.toMap)
    tree

  }

  /* Wrapper to the TANTree.predict function which holds all the data needed for prediction */
  def predict(attr: String, predictor: Int): Array[(String, Double)] = {

    tree.predict(attr, predictor)
  }
}