import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class FPTree {
  var root: FPNode = FPNode(this, None, None)
  var route = None: Option[(FPNode, FPNode)]
  val routes: mutable.Map[Option[String], (FPNode, FPNode)] = mutable.Map.empty

  def add(transaction: ListBuffer[String]): Unit = {
    var point = root
    var nextPoint = root

    for (item <- transaction) {
      if (point.contains(item)) {
        nextPoint = point.search(item).get
        nextPoint.increment()
      }
      else {
        nextPoint = FPNode(this, Option(item), Option(point))
        this.updateRoute(nextPoint)
      }
      point = nextPoint
    }
  }

  def updateRoute(point: FPNode): Unit = {
    if (point.tree != this) {
      throw new IllegalArgumentException("Given point is not from the current tree.")
    }
    if (routes.contains(point.item)) {
      routes(point.item)._2.neighbour = Option(point)
      routes(point.item) = (route.get._1, point)
    }
    else {
      // First node for this item; start a new route
      routes(point.item) = (point, point)
    }
  }

  def items(): ListBuffer[(String, ListBuffer[FPNode])] = {
    val items = ListBuffer[(String, ListBuffer[FPNode])]()
    for (item <- routes.keys) {
      items += ((item.get, this.nodes(item)))
    }
    items
  }

  def nodes(item: Option[String]): ListBuffer[FPNode] = {
    val nodes = ListBuffer[FPNode]()
    if (routes.contains(item)) {
      var node = Option(routes(item)._1)
      while (node.isDefined) {
        nodes += node.get
        node = node.get.neighbour
      }
    }
    nodes
  }

  def prefixPaths(item: Option[String]): ListBuffer[ListBuffer[FPNode]] = {
    val paths = ListBuffer[ListBuffer[FPNode]]()
    for (node <- this.nodes(item)) {
      var path = ListBuffer[FPNode]()
      var n = Option(node)
      while (n.isDefined && !n.get.root) {
        path += n.get
        n = n.get.parent
      }
      paths += path
    }
    paths
  }

  def inspect(): Unit = {
    println("Tree:")
    root.inspect(1)
    println("\nRoutes:")
    for (itemNodes <- this.items()) {
      println(s"  $itemNodes._1")
      for (node <- itemNodes._2) {
        println(s"    $node.repr()")
      }
    }
  }
}
