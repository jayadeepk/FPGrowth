import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.math.Ordering.Implicits._

object FPGrowth {

  def main(args: Array[String]): Unit = {
    val startTime = System.nanoTime()
    val frequentItemsets = findFrequentItemsets("res/data.csv", 3)
    val endTime = System.nanoTime()
    println("Elapsed time: " + (endTime - startTime)/1000000 + "ms\n")

    for (freqItemset <- frequentItemsets.sortBy(_._1)) {
      println("(" + freqItemset._1.mkString(",") + ") " + freqItemset._2)
    }
  }


  def findFrequentItemsets(transactionsPath: String, minSupport: Int): ListBuffer[(ListBuffer[String], Int)] = {
    val itemsets = ListBuffer[(ListBuffer[String], Int)]()
    val items = mutable.Map[String, Int]()

    // Get transactions from csv
    var csv = io.Source.fromFile(transactionsPath)

    // Iterate line by line instead of reading whole file
    for (line <- csv.getLines) {
      for (item <- line.split(",")) {
        if (items.contains(item))
          items(item) += 1
        else
          items(item) = 1
      }
    }

    // Create item support map and populate with frequent 1-itemsets
    val frequentItems = items filter ( _._2 >= minSupport )

    // Add filtered and sorted transactions to master FPTree
    val master = new FPTree()
    csv = io.Source.fromFile(transactionsPath)
    for (line <- csv.getLines) {
      master.addTransaction(
        line.split(",").toList
            .filter(frequentItems.contains)
            .sortBy(x => (-frequentItems(x), x.toInt)))
    }
    itemsets ++= findWithSuffix(master, ListBuffer[String](), minSupport)
//    master.inspect()
    itemsets
  }


  def findWithSuffix(tree: FPTree, suffix: ListBuffer[String], minSupport: Int): ListBuffer[(ListBuffer[String], Int)] = {
    val itemsets = ListBuffer[(ListBuffer[String], Int)]()
    for (itemNodes <- tree.items()) {
      val item = itemNodes._1
      val nodes = itemNodes._2
      val support = nodes.map(_.count).sum

      if (support >= minSupport && !suffix.contains(item)) {
        itemsets += Tuple2(item +: suffix, support)

        val condTree = conditionalTree(tree.prefixPaths(Option(item)))
        itemsets ++= findWithSuffix(condTree, item +: suffix, minSupport)
      }
    }
    itemsets
  }


  def conditionalTree(paths: ListBuffer[ListBuffer[FPNode]]): FPTree = {
    val tree = new FPTree()
    var conditionItem: Option[String] = None

    for (path <- paths) {
      if (conditionItem.isEmpty) {
        conditionItem = path.last.item
      }

      var point = tree.root
      for (node <- path) {
        var nextPoint = point.search(node.item.get)
        if (nextPoint.isEmpty) {
          val count: Int = if (node.item == conditionItem) node.count else 0
          nextPoint = Option(FPNode(tree, node.item, Option(point), count))
          tree.updateRoute(nextPoint.get)
        }
        point = nextPoint.get
      }
    }

    if (conditionItem.isEmpty) {
      throw new Exception("conditionItem is empty.")
    }

    for (path <- tree.prefixPaths(conditionItem)) {
      val count = path.last.count
      for (node <- path.dropRight(1)) {
        node.count += count
      }
    }
    tree
  }
}
