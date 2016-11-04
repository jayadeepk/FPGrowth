import scala.collection.mutable

case class FPNode(var tree: FPTree, var item: Option[String],
                  var parent: Option[FPNode], var count: Int = 1) {

  val children: mutable.Map[String, FPNode] = mutable.Map.empty
  var neighbour = None: Option[FPNode]

  // Add this as a child to the parent
  if (parent.isDefined && item.isDefined) {
    parent.get.children += (item.get -> this)
  }

  // Return true if this node is root
  def root: Boolean = parent.isEmpty

  // Search for given item in one of the children
  def search(item: String): Option[FPNode] = {
    children.get(item)
  }

  // Return true if one of the children contains given item
  def contains(item: String): Boolean = {
    children.contains(item)
  }

  // Increment items frequency
  def increment(): Unit = {
    count += 1
  }

  // Return true if the node is a leaf
  def leaf: Boolean = children.isEmpty

  // Set parent of current node
  def setParent(newParent: FPNode): Unit = {
    if (newParent.tree != tree) {
      throw new IllegalArgumentException("Only nodes in current tree can be added as parent.")
    }
    parent = Option(newParent)
  }

  // Set the neighbour (to the right) of the current node in FPTree
  def setNeighbour(newNeighbour: FPNode): Unit = {
    if (newNeighbour.tree != tree) {
      throw new IllegalArgumentException("Only nodes in the current tree can be added as neighbour.")
    }
    neighbour = Option(newNeighbour)
  }

  def inspect(depth: Int = 0): Unit = {
    println("  " * depth + repr())
    children.foreach(_._2.inspect(depth+1))
  }

  def repr(): String = {
    if (root) {
      "<root>"
    }
    else {
      item.get + "(" + count + ")"
    }
  }
}
