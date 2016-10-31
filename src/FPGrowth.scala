import scala.collection.mutable.{HashMap, ListBuffer}

object FPGrowth {
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
  }
}
