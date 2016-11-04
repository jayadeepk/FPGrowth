import scala.collection.mutable.ListBuffer

object FPGrowth {
  def cleanTransaction(transaction: ListBuffer[String], items: Map[String, Int]): ListBuffer[String] = {
    // Remove infrequent items from transactions
    val cleanedTransaction = ListBuffer[String]() ++ transaction.filter(items.contains)
    cleanedTransaction
  }

  def findWithSuffix(tree: FPTree, suffix: ListBuffer[String], minSupport: Int): ListBuffer[ListBuffer[String]] = {
    val itemsets = ListBuffer[ListBuffer[String]]()
    for (itemNodes <- tree.items()) {
      val item = itemNodes._1
      val nodes = itemNodes._2
      val support = nodes.map(_.count).sum

      if (support >= minSupport && !suffix.contains(item)) {
        itemsets += item +: suffix

        val condTree = conditionalTree(tree.prefixPaths(Option(item)))
        itemsets ++= findWithSuffix(condTree, item +: suffix, minSupport)
      }
    }
    itemsets
  }

  def findFrequentItemsets(transactionsPath: String, minSupport: Int): ListBuffer[ListBuffer[String]] = {
    val itemsets = ListBuffer[ListBuffer[String]]()
    // Get transactions from csv
    val csv = io.Source.fromFile(transactionsPath)
    val transactions = ListBuffer[ListBuffer[String]]()
    for (line <- csv.getLines) {
      val transaction = ListBuffer[String]()
      for (col <- line.split(",")) {
        transaction += col
      }
      transactions += transaction
    }

    // Create item support map and populate with frequent 1-itemsets
    val items: Map[String, Int] = Map[String, Int]() ++ (
      transactions.flatten
        groupBy identity
        mapValues { _.size }
        filter { _._2 >= minSupport }
    )

    // Clean transactions
    val cleanedTransactions = ListBuffer[ListBuffer[String]]()
    transactions.foreach(
      cleanedTransactions += cleanTransaction(_, items)
    )

    val master = new FPTree()

    for (transaction <- cleanedTransactions) {
      master.add(transaction)
    }

    itemsets ++ findWithSuffix(master, ListBuffer[String](), minSupport)
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
          val count = if(node.item == conditionItem) node.count else 0
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

  def main(args: Array[String]): Unit = {
    findFrequentItemsets("res/data.csv", 2) sortBy (_.head) foreach println
  }
}
