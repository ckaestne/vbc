package edu.cmu.cs.vbc.utils

/**
  * Collect and print statistics
  *
  * @author chupanw
  */
object Statistics {

  /**
    * Collect statistics output using a StringBuilder
    */
  val printer = new StringBuilder

  def header(name: String) = s"********** $name **********"

  /**
    * Collect
    *
    * @param methodName
    * @param nLifting
    * @param nTotal
    */
  def collectLiftingRatio(methodName: String, nLifting: Int, nTotal: Int) = {
    printer.append(String.format("%-50s %10s", "\tMethod " + methodName + ":", nLifting + "/" + nTotal + "\n"))
  }

  def printStatistics(): Unit = {
    println("\n\n")
    println(header("Lifting Ratio"))
    println(printer.toString)
  }
}
