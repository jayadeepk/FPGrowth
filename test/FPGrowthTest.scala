import java.io.{File, PrintWriter}

import scala.collection.mutable.ListBuffer
import scala.util.Random
import scala.math.Ordering.Implicits._

object FPGrowthTest {
  def main(args: Array[String]): Unit = {
    val minSup = 2
    val numTransactions = 10
    val numItems = 5
    val numTests = 10
    var testCasesPassed = 0
    refreshLogs()

    for (i <- 1 to numTests) {
      val data = generateData(numTransactions, numItems)
      writeData(data, "res/testdata.csv")
//          val data =  readData("res/testdata.csv")

      val freqItemsets = FPGrowth.findFrequentItemsets("res/testdata.csv", minSup)
      //    printItemsets(freqItemsets)
      writeItemsets(freqItemsets, "res/output.txt")

      val freqItemsetsTest = frequentItemsets(data, numTransactions, numItems, minSup)
      //    printItemsets(freqItemsetsTest)
      writeItemsets(freqItemsetsTest, "res/testoutput.txt")

      if (testPassed("res/output.txt", "res/testoutput.txt")) {
        testCasesPassed += 1
        log(data, freqItemsets, freqItemsetsTest, i, "passed")
      }
      else {
        log(data, freqItemsets, freqItemsetsTest, i, "failed")
      }
    }
    println("Test cases passed: " + testCasesPassed + "/" + numTests)
  }

  def frequentItemsets(data: ListBuffer[ListBuffer[String]], numTransactions: Int,
                       numItems: Int, minSup: Int): ListBuffer[(ListBuffer[String], Int)] = {

    val itemsets = ListBuffer[(ListBuffer[String], Int)]()
    val set = List.range(0, numItems).map(_.toString).toSet

    for (subset <- set.subsets) {
      if (subset.nonEmpty) {
        var support = 0
        for (transaction <- data) {
          if (subset.subsetOf(transaction.toSet)) {
            support += 1
          }
        }
        if (support >= minSup) {
          itemsets += ((subset.to[ListBuffer], support))
        }
      }
    }
    itemsets
  }

  // Generate synthetic data
  def generateData(numTransactions: Int, numItems: Int): ListBuffer[ListBuffer[String]] = {
    val data = ListBuffer[ListBuffer[String]]()

    for (i <- 1 to numTransactions) {
      // Choose random length of transaction
      val random = Random
      val length = 3 + random.nextInt(numItems-2)
      var transaction = Set[String]()

      for (j <- 1 to length) {
        transaction += random.nextInt(numItems).toString
      }
      data += transaction.to[ListBuffer]
    }
    data
  }

  def printData(data: ListBuffer[ListBuffer[String]]): Unit = {
    for (transaction <- data) {
      for (item <- transaction) {
        print(item + " ")
      }
      println
    }
  }

  def writeData(data: ListBuffer[ListBuffer[String]], outputPath: String): Unit = {
    val pw = new PrintWriter(new File(outputPath))
    for (transaction <- data) {
      pw.write(transaction.mkString(",") + "\n")
    }
    pw.close()
  }

  def readData(inputPath: String): ListBuffer[ListBuffer[String]] = {
    val data = ListBuffer[ListBuffer[String]]()
    val dataFile = io.Source.fromFile(inputPath)

    for (line <- dataFile.getLines()) {
      data += line.split(",").to[ListBuffer]
    }
    data
  }

  def printItemsets(itemsets: ListBuffer[(ListBuffer[String], Int)]): Unit = {
    for (itemset <- itemsets.sortBy(_._1)) {
      println("(" + itemset._1.mkString(",") + ") " + itemset._2)
    }
  }

  def writeItemsets(itemsets: ListBuffer[(ListBuffer[String], Int)], outputPath: String): Unit = {
    val pw = new PrintWriter(new File(outputPath))
    val sorted = ListBuffer[(ListBuffer[String], Int)]()
    for (itemset <- itemsets) {
      sorted += ((itemset._1.sortBy(_.toInt), itemset._2))
    }
    for (itemset <- sorted.sortBy(_._1)) {
      pw.write("(" + itemset._1.sortBy(_.toInt).mkString(",") + ") " + itemset._2 + "\n")
    }
    pw.close()
  }

  def testPassed(file1: String, file2: String): Boolean = {
    val lines1 = io.Source.fromFile(file1).getLines().toList
    val lines2 = io.Source.fromFile(file2).getLines().toList

    var failed = false
    var failedLine = -1

    for (i <- 0 to lines1.size) {
      if (lines1 != lines2) {
        failed = true
        failedLine = i + 1
      }
    }
    !failed
  }

  def refreshLogs(): Unit = {
    for {
          files <- Option(new File("res/logs/passed").listFiles)
          file <- files if file.getName.endsWith(".log")
        } file.delete()

    for {
          files <- Option(new File("res/logs/failed").listFiles)
          file <- files if file.getName.endsWith(".log")
        } file.delete()
  }

  def log(data: ListBuffer[ListBuffer[String]], freqItemsets: ListBuffer[(ListBuffer[String], Int)],
          freqItemsetsTest: ListBuffer[(ListBuffer[String], Int)], i: Int, pass: String): Unit = {

        writeData(data, "res/logs/" + pass + "/data" + i + ".log")
        writeItemsets(freqItemsets, "res/logs/" + pass + "/output" + i + ".log")
        writeItemsets(freqItemsetsTest, "res/logs/" + pass + "/testoutput" + i + ".log")
  }
}
