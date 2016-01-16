package edu.cmu.cs.vbc

import org.objectweb.asm.ClassReader
import org.objectweb.asm.tree.{AbstractInsnNode, ClassNode, InsnList}

import scala.collection.JavaConversions._
import scala.collection.mutable

/**
  * Created by ckaestne on 1/15/2016.
  */
object CFGAndStackAnalysis extends App {

    val cr = new ClassReader("edu.cmu.cs.vbc.VClassLoader")
    //    val cr = new ClassReader("org.objectweb.asm.ClassReader")

    val cn = new ClassNode()
    cr.accept(cn, 0)

    for (method <- cn.methods) {
        //        println(method.instructions)

        val a = new StackHeightAnalysis()
        a.analyze(cn.name, method)

        //        for (i <- 0 until a.getFrames.size - 1) {
        //            val in = OpcodePrint.print(method.instructions.get(i).getOpcode)
        //            println(i + ": " + in + " " + a.getStackDiff(method.instructions.get(i)))
        //        }


        val cfg = toCFG(getCFG(a), method.instructions)

        if (cfg.hasCycles)
            println("cycles: " + method.name + ":\n" + cfg.nodes.mkString("\n"))

        //        if (cfg.nodes.exists(_.dependsOnStackValues)) {
        //            println(method.name)
        //            for (n <- cfg.nodes) {
        //                print(n)
        //                print(" " + n.stackDiff)
        //                if (n.dependsOnStackValues)
        //                    print(" !!")
        //                println()
        //            }
        //        }
    }


    def getCFG(a: StackHeightAnalysis): Set[_CFGNode] = {
        val instructionToNodeMap: mutable.Map[Int, _CFGNode] = mutable.Map()

        def getNode(inst: Int): _CFGNode = {
            instructionToNodeMap.getOrElseUpdate(inst, {
                val n = new _CFGNode
                n.instructions = n.instructions :+ inst
                n
            })
        }
        def mergeNodes(from /*only one successor*/ : _CFGNode, into: _CFGNode): Unit = {
            assert(from.succInstr.size <= 1)
            into.instructions = from.instructions ++ into.instructions
            from.instructions.foreach(inst => instructionToNodeMap.put(inst, into))
        }

        for ((inst, succs) <- a.succ) {
            val node = getNode(inst)
            def countPred(i: Int) = a.succ.values().count(_ contains i)

            if (succs.size() == 1 && countPred(succs.head) <= 1)
                mergeNodes(node, getNode(succs.head))
            else
                node.succInstr = succs.toSet[Integer].map(_.toInt)
        }

        instructionToNodeMap.values.toSet
    }


    def toCFG(internalNodes: Set[_CFGNode], insts: InsnList): CFG = {
        val nodes = internalNodes.toList.sortBy(_.instructions.min)
        var result: List[CFGNode] = Nil
        for (i <- 0 until nodes.size) {
            val node = nodes(i)
            result = result :+ new CFGNode(i, node.instructions.map(insts.get).map(i => (i, (MyFrame.elementsPoppedMap.get(i), MyFrame.elementsPushedMap.get(i)))))
        }
        def findInstr(i: Int) = nodes.indexWhere(_.instructions contains i)
        for (i <- 0 until nodes.size)
            result(i).succ = nodes(i).succInstr.map(i => result(findInstr(i)))
        for (n <- result)
            n.succ.foreach(_.pred += n)
        new CFG(result)
    }

    /** Internal representation for computing the graph */
    class _CFGNode {
        var instructions: List[Int] = Nil
        var succInstr: Set[Int] = Set()

        override def toString =
            instructions.mkString("[", ",", "]->") +
                succInstr.mkString("[", ",", "]")

    }


    class CFG(var nodes: List[CFGNode]) {


        def hasCycles: Boolean = {

            def visit(n: CFGNode, knownNodes: Set[CFGNode]): Boolean = {
                if (knownNodes contains n)
                    false
                else
                    n.succ.map(visit(_, knownNodes + n)).fold(true)(_ && _)
            }

            !visit(nodes.head, Set())
        }

    }

    /** CFG representation for analysis */
    class CFGNode(val nodeIdx: Int, instructions: List[(AbstractInsnNode, (Integer, Integer))]) {
        var succ: Set[CFGNode] = Set()
        var pred: Set[CFGNode] = Set()

        override def toString =
            "#" + nodeIdx + ":" +
                instructions.filter(_._1.getOpcode >= 0).map(inst => OpcodePrint.print(inst._1.getOpcode) + "[-" + inst._2._1 + ",+" + inst._2._2 + "]").mkString("[", ",", "]->") +
                succ.map(s => "#" + s.nodeIdx).mkString("[", ",", "]")

        def dependsOnStackValues: Boolean = {
            var stackHeight = 0
            for ((_, (pop, push)) <- instructions) {
                if (pop != null)
                    stackHeight -= pop
                if (stackHeight < 0)
                    return true
                if (push != null)
                    stackHeight += push
            }
            return false
        }

        def stackDiff: Int = {
            var stackHeight = 0
            for ((_, (pop, push)) <- instructions) {
                if (pop != null)
                    stackHeight -= pop
                if (push != null)
                    stackHeight += push
            }
            stackHeight
        }

    }

}
