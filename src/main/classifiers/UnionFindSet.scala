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