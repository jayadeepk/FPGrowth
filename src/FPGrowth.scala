import scala.collection.mutable.ListBuffer

object FPGrowth {
  def cleanTransaction(transaction: ListBuffer[String], items: Map[String, Int]): ListBuffer[String] = {
    // Remove infrequent items from transactions
    val cleanedTransaction = ListBuffer[String]() ++ transaction.filter(items.contains)
    cleanedTransaction
  }

  def findFrequentItemsets(transactionsPath: String, minSupport: Int): Unit = {

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
  }
}
