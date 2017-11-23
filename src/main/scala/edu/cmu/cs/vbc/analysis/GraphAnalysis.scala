package edu.cmu.cs.vbc.analysis

object GraphAnalysis {

  def computeDominators[Node](nodes: Set[Node], first: Node, pred: Node => Set[Node]): Map[Node, Set[Node]] = {
    assert(nodes contains first)
    var result = Map(first -> Set(first))

    for (node <- nodes; if node != first)
      result += (node -> nodes)

    var changes = true
    while (changes) {
      changes = false
      for (node <- nodes; if node != first) {
        val oldSet = result(node)
        val newSet = Set(node) union (
          pred(node).foldLeft[Set[Node]](nodes)(_ intersect result(_))
          )
        if (oldSet != newSet) {
          changes = true
          result += (node -> newSet)
        }
      }
    }
    result
  }

  def computePostDominators[Node](nodes: Set[Node], last: Node, succ: Node => Set[Node]): Map[Node, Set[Node]] =
    computeDominators(nodes, last, succ)

  def strictDominators[Node](dominators: Map[Node, Set[Node]]): Map[Node, Set[Node]] =
    dominators.map(entry => (entry._1 -> (entry._2 - entry._1)))

  def immediateDominators[Node](strictDominators: Map[Node, Set[Node]]): Map[Node, Node] = {
    var result = Map[Node, Node]()

    def isImmediate(candiate: Node, otherDom: Set[Node]): Boolean =
      !(otherDom.flatMap(strictDominators.apply) contains candiate)

    for ((n, dom) <- strictDominators) {
      dom.find(c => isImmediate(c, dom - c)).map(c => result += (n -> c))
    }
    result
  }


  def computeImmediatePostDominators[Node](nodes: Set[Node], last: Node, succ: Node => Set[Node]): Map[Node, Node] =
    immediateDominators(strictDominators(computeDominators(nodes, last, succ)))


}
