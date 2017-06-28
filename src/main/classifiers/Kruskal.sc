import scala.collection.immutable.Vector
import scala.util.Sorting

//class Node[T](val value: T) {
//  var rank: Integer = 0
//  var parent = this
//
//  override def toString = {
//    var node = this
//    var string = ""
//    while (node != node.parent) {
//      string += node.value + "[" + node.rank + "]" + "->"
//      node = node.parent
//    }
//    string + String.valueOf(node.value) + "[" + node.rank + "]"
//  }
//
//}
//
//def find[T](x: Node[T]): Node[T] ={
//  if (x.parent == x) x else find(x.parent)
//}
//
//def union[T](x: Node[T], y: Node[T]): Node[T] = {
//  val rx = find(x)
//  val ry = find(y)
//  if (rx == ry){
//    return rx
//  }
//  if (rx.rank > ry.rank) {
//    ry.parent = rx
//    rx
//  } else {
//    rx.parent = ry
//    if (rx.rank == ry.rank) {
//      ry.rank = ry.rank + 1
//    }
//    ry
//  }
//}
trait UnionFindSet[T] {
  var parent: UnionFindSet[T]
  var rank: Integer
  val value: T
}

object UnionFindSet {
  private class Node[T](val value: T) extends UnionFindSet[T]{
    var rank: Integer = 0
    var parent: UnionFindSet[T] = this

    override def toString = {
      var node: UnionFindSet[T] = this
      var string = ""
      while (node != node.parent) {
        string += node.value + "[" + node.rank + "]" + "->"
        node = node.parent
      }
      string + String.valueOf(node.value) + "[" + node.rank + "]"
    }

  }

  def makeSet[T](value: T): UnionFindSet[T] ={
    new Node(value)
  }
  def find[T](x: UnionFindSet[T]): UnionFindSet[T] ={
    if (x.parent == x) x else find(x.parent)
  }

  def union[T](x: UnionFindSet[T], y: UnionFindSet[T]): UnionFindSet[T] = {
    val rx = find(x)
    val ry = find(y)
    if (rx == ry){
      return rx
    }
    if (rx.rank > ry.rank) {
      ry.parent = rx
      rx
    } else {
      rx.parent = ry
      if (rx.rank == ry.rank) {
        ry.rank = ry.rank + 1
      }
      ry
    }
  }

}


def buildUndirectedGraph(): Vector[Vector[Double] ] = {
  val size = 5
  val rowBuilder = Vector.newBuilder[Double]
  val matrixBuilder = Vector.newBuilder[Vector[Double]]
  var i, j = (0, 0)
  for (i <- 0 to size-1) {
    for (j <- 0 to size-1) {
      rowBuilder += (.0+i+j)/size
    }
    matrixBuilder += rowBuilder.result()
    rowBuilder.clear()
  }
  matrixBuilder.result()
}

def edgeList(weightMatrix: Vector[Vector[Double]], i: Int, j: Int, list: List[(Int, Int, Double)]): List[(Int, Int, Double)] = {
    if (i < weightMatrix.size) {
      if (j == weightMatrix.size) {
        edgeList(weightMatrix, i+1, i+1, list)
      } else {
        edgeList(weightMatrix, i, j+1, list ++ List( (i,j, weightMatrix(i)(j) ) ))
      }
    } else {
      Sorting.stableSort(list, (x: (Int,Int,Double),y: (Int,Int,Double)) => x._3 > y._3).toList
    }
  }

val weightMatrix: Vector[Vector[Double]] = buildUndirectedGraph()
weightMatrix.foreach(println)
val edges = edgeList(weightMatrix, 0,0, List())

val a = UnionFindSet.makeSet("A")
val b = UnionFindSet.makeSet("B")
val c = UnionFindSet.makeSet("C")
val d = UnionFindSet.makeSet("D")
val e = UnionFindSet.makeSet("E")
val f = UnionFindSet.makeSet("F")
val g = UnionFindSet.makeSet("G")

val F = List(a,b,c,d,e)
var A = Array.tabulate(5,5) ( (_, _) => false)

def construct(edges: List[(Int,Int,Double)]) {
  if (edges.nonEmpty) {
    val (i, j, w) = edges.head
    if (UnionFindSet.find(F(i)) != UnionFindSet.find(F(j))) {
      UnionFindSet.union(F(i), F(j))
      A(i)(j) = true
    }
    construct(edges.tail)
  }
}

construct(edges)
//
//val u = union(a,d)
//val u2 = union(b,e)
//val u3 = union(c,f)
//
//
//union(c,g)
//union(e,a)
//union(b,g)

println(a)
println(b)
println(c)
println(d)
println(e)
println(f)
println(g)
A.foreach( (e) => e.foreach(println))

//val n2 = UnionFindSet.makeSet(3)
//
//val u = union(n, n2)
//
//u.value
//u.rank
//u.parent.value
//  def this(val value: T): Tree[T] = {
//    this.parent = this
//    this.rank: Integer = 0
//
//    this
//  }
//
//  def find(x: Tree[T]): Tree[T] = {
//    if (x.parent == x) x else find(x.parent)
//  }
//
//  def union(x: Tree[T], y: Tree[T]) = {
//    var xRoot = find(x)
//    val yRoot = find(y)
//    if xRoot == yRoot:
//      return
//    if (xRoot.rank > yRoot.rank)
//
//  }
//}
