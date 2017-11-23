package edu.cmu.cs.vbc.analysis

import org.scalatest.FunSuite

class GraphAnalysisTest extends FunSuite {

  test("simple") {
    //wikipedia example
    val nodes = Set(1, 2, 3, 4, 5, 6)
    val succ = Map(1 -> Set(2), 2 -> Set(3, 4, 6), 3 -> Set(5), 4 -> Set(5), 5 -> Set(2))
    val pred = Map(2 -> Set(1, 5), 3 -> Set(2), 4 -> Set(2), 5 -> Set(3, 4), 6 -> Set(2))

    import GraphAnalysis._
    println(computeDominators[Int](nodes, 1, pred.apply))
    println(strictDominators(computeDominators[Int](nodes, 1, pred.apply)))
    assert(
      immediateDominators(strictDominators(computeDominators[Int](nodes, 1, pred.apply))) ==
        Map(5 -> 2, 6 -> 2, 2 -> 1, 3 -> 2, 4 -> 2))

    println(computePostDominators[Int](nodes, 6, succ.apply))
    println(strictDominators(computePostDominators[Int](nodes, 6, succ.apply)))
    assert(
      immediateDominators(strictDominators(computePostDominators[Int](nodes, 6, succ.apply))) ==
        Map(5 -> 2, 1 -> 2, 2 -> 6, 3 -> 5, 4 -> 5))
  }

}
