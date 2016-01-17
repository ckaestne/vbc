package edu.cmu.cs.vbc.instructions

import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{ClassVisitor, Label, MethodVisitor, Type}


trait Instruction extends LiftUtils {
    def toByteCode(mv: MethodVisitor, method: MethodNode)

    def toVByteCode(mv: MethodVisitor, method: MethodNode)

    def getVariables: Set[Integer] = Set()
}

case class InstrIADD() extends Instruction {
    override def toByteCode(mv: MethodVisitor, method: MethodNode): Unit = {
        mv.visitInsn(IADD)
    }

    override def toVByteCode(mv: MethodVisitor, method: MethodNode): Unit = {
        mv.visitMethodInsn(INVOKESTATIC, vopsclassname, "IADD", "(Ledu/cmu/cs/varex/V;Ledu/cmu/cs/varex/V;)Ledu/cmu/cs/varex/V;", false)
    }
}

case class InstrICONST(v: Int) extends Instruction {
    override def toByteCode(mv: MethodVisitor, method: MethodNode): Unit = {
        writeConstant(mv, v)
    }

    override def toVByteCode(mv: MethodVisitor, method: MethodNode): Unit = {
        writeConstant(mv, v)
        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false)
        mv.visitMethodInsn(INVOKESTATIC, vclassname, "one", "(Ljava/lang/Object;)Ledu/cmu/cs/varex/V;", true)
    }
}


case class InstrRETURN() extends Instruction {
    override def toByteCode(mv: MethodVisitor, method: MethodNode): Unit =
        mv.visitInsn(RETURN)

    override def toVByteCode(mv: MethodVisitor, method: MethodNode): Unit =
        mv.visitInsn(RETURN)
}



case class InstrIINC(variable: Int, increment: Int) extends Instruction {
    override def toByteCode(mv: MethodVisitor, method: MethodNode): Unit =
        mv.visitIincInsn(variable, increment)

    override def toVByteCode(mv: MethodVisitor, method: MethodNode): Unit = {
        mv.visitVarInsn(ALOAD, variable + ctxParameterOffset)
        writeConstant(mv, increment)
        mv.visitMethodInsn(INVOKESTATIC, vopsclassname, "IINC", "(Ledu/cmu/cs/varex/V;I)Ledu/cmu/cs/varex/V;", false)
        mv.visitVarInsn(ASTORE, variable + ctxParameterOffset)
    }

    override def getVariables() = Set(variable)
}

case class InstrISTORE(variable: Int) extends Instruction {
    override def toByteCode(mv: MethodVisitor, method: MethodNode): Unit =
        mv.visitVarInsn(ISTORE, variable)

    override def toVByteCode(mv: MethodVisitor, method: MethodNode): Unit = {
        mv.visitVarInsn(ASTORE, variable + ctxParameterOffset)
    }

    override def getVariables() = Set(variable)
}

case class InstrILOAD(variable: Int) extends Instruction {
    override def toByteCode(mv: MethodVisitor, method: MethodNode): Unit =
        mv.visitVarInsn(ILOAD, variable)

    override def toVByteCode(mv: MethodVisitor, method: MethodNode): Unit = {
        mv.visitVarInsn(ALOAD, variable + ctxParameterOffset)
    }

    override def getVariables() = Set(variable)
}

/**
  * assumptions (for now)
  *
  * the if statement is the last statement in a block, making a decision
  * between the next block or the referenced block
  *
  * for now, jumps can only be made forward, not backward (loops not yet
  * supported)
  *
  * for now, blocks need to be balanced wrt to the stack (not enforced yet)
  */
case class InstrIFEQ(targetBlockIdx: Int) extends Instruction {


    override def toByteCode(mv: MethodVisitor, method: MethodNode): Unit = {
        val cfg = method.body
        val targetBlock = cfg.blocks(targetBlockIdx)
        val thisBlockIdx = findThisBlock(method).idx
        assert(targetBlockIdx > thisBlockIdx, "not supporting backward jumps yet")
        assert(targetBlockIdx < cfg.blocks.size, "attempting to jump beyond the last block")
        assert(thisBlockIdx < cfg.blocks.size - 1, "attempting to jump from the last block")

        mv.visitJumpInsn(IFEQ, targetBlock.label)
    }

    override def toVByteCode(mv: MethodVisitor, method: MethodNode): Unit = {
        val cfg = method.body
        val thisBlock = findThisBlock(method)

        /**
          * creating a variable for the decision
          *
          * on top of the stack is the condition, which should be V[Int];
          * that is, we want to know when that value is different from 0
          *
          * the condition is then stored as feature expression in a new
          * variable. this variable is used at the beginning of the relevant
          * blocks to modify the ctx
          *
          * the actual modification of ctx happens Block.toVByteCode
          */
        //        println("creating variable " + thisBlock.getBlockDecisionVar() + " for block " + thisBlock.idx)

        mv.visitMethodInsn(INVOKESTATIC, vopsclassname, "whenEQ", "(Ledu/cmu/cs/varex/V;)Lde/fosd/typechef/featureexpr/FeatureExpr;", false)
        //        mv.visitInsn(DUP)
        //        mv.visitMethodInsn(INVOKESTATIC, "edu/cmu/cs/vbc/test/TestOutput", "printFE", "(Lde/fosd/typechef/featureexpr/FeatureExpr;)V", false)
        mv.visitVarInsn(ASTORE, thisBlock.blockDecisionVar)


    }
}


case class InstrGOTO(targetBlockIdx: Int) extends Instruction {
    override def toByteCode(mv: MethodVisitor, method: MethodNode): Unit = {
        val cfg = method.body
        val targetBlock = cfg.blocks(targetBlockIdx)
        val thisBlockIdx = findThisBlock(method).idx
        assert(targetBlockIdx > thisBlockIdx, "not supporting backward jumps yet")
        assert(targetBlockIdx < cfg.blocks.size, "attempting to jump beyond the last block")
        assert(thisBlockIdx < cfg.blocks.size - 1, "attempting to jump from the last block")

        mv.visitJumpInsn(GOTO, targetBlock.label)
    }

    override def toVByteCode(mv: MethodVisitor, method: MethodNode): Unit = {

    }

}

case class Block(instr: Instruction*) extends LiftUtils {
    var label: Label = null

    def toByteCode(mv: MethodVisitor, method: MethodNode) = {
        mv.visitLabel(label)
        instr.foreach(_.toByteCode(mv, method))
    }

    def toVByteCode(mv: MethodVisitor, method: MethodNode) = {
        mv.visitLabel(label)

        //TODO: optimize and do not recreate ctx if the previous block has the same condition

        //load ctx parameter the method received
        mv.visitVarInsn(ALOAD, 1)
        //load variables that store relevant previous decisions and connect them with ctx
        //using and and andNot
        val blockCondition = getBlockCondition()
        if (blockCondition.pathConditions.size < 2)
            for (pathCond <- blockCondition.pathConditions;
                 decision <- pathCond.decisions) {
                mv.visitVarInsn(ALOAD, decision.idx)
                mv.visitMethodInsn(INVOKEINTERFACE, fexprclassname, if (decision.negated) "andNot" else "and", "(Lde/fosd/typechef/featureexpr/FeatureExpr;)Lde/fosd/typechef/featureexpr/FeatureExpr;", true)
            }
        else {
            var first = true
            for (pathCond <- blockCondition.pathConditions) {
                mv.visitMethodInsn(INVOKESTATIC, "de/fosd/typechef/featureexpr/FeatureExprFactory", "True", "()Lde/fosd/typechef/featureexpr/FeatureExpr;", false)
                for (decision <- pathCond.decisions) {
                    mv.visitVarInsn(ALOAD, decision.idx)
                    mv.visitMethodInsn(INVOKEINTERFACE, fexprclassname, if (decision.negated) "andNot" else "and", "(Lde/fosd/typechef/featureexpr/FeatureExpr;)Lde/fosd/typechef/featureexpr/FeatureExpr;", true)
                }
                if (!first)
                    mv.visitMethodInsn(INVOKEINTERFACE, fexprclassname, "or", "(Lde/fosd/typechef/featureexpr/FeatureExpr;)Lde/fosd/typechef/featureexpr/FeatureExpr;", true)
                first = false
            }
            mv.visitMethodInsn(INVOKEINTERFACE, fexprclassname, "and", "(Lde/fosd/typechef/featureexpr/FeatureExpr;)Lde/fosd/typechef/featureexpr/FeatureExpr;", true)
        }

        //set as ctx variable
        mv.visitVarInsn(ASTORE, method.ctxLocalVar)


        println("starting block " + idx + " with condition " + getBlockCondition())
        instr.foreach(_.toVByteCode(mv, method))
    }


    def isConditionalBlock() = instr.lastOption.map(_.isInstanceOf[InstrIFEQ]).getOrElse(false)


    var successors: List[Block] = Nil
    var predecessors: Set[Block] = Set()
    var idx: Int = -1

    /**
      * returns successors (none if last,
      * two if block ends with an if statement
      * of which the second is the target of the if statement,
      * one otherwise)
      */
    private[instructions] def computeSuccessors(cfg: CFG) = {
        val thisBlockIdx = cfg.blocks.indexOf(this)
        if (thisBlockIdx + 1 < cfg.blocks.size) {
            if (isConditionalBlock())
                successors = List(cfg.blocks(thisBlockIdx + 1), cfg.blocks(instr.last.asInstanceOf[InstrIFEQ].targetBlockIdx))
            else if (instr.lastOption.map(_.isInstanceOf[InstrGOTO]).getOrElse(false))
                successors = List(cfg.blocks(instr.last.asInstanceOf[InstrGOTO].targetBlockIdx))
            else
                successors = List(cfg.blocks(thisBlockIdx + 1))
        } else
            successors = Nil
    }

    /** call after computing all successors */
    private[instructions] def computePredecessors(cfg: CFG) = {
        predecessors = for (block <- cfg.blocks.toSet;
                            if block.successors.filter(_ eq this).nonEmpty)
            yield block
    }

    /**
      * variable that refers to a condition/featureExpr
      * regarding whether the first or the second
      * successor block is chosen
      */
    var blockDecisionVar = -1


    private def getBlockCondition(): BlockCondition = {
        val conditions = for (pred <- predecessors) yield {
            val c = pred.getBlockCondition()
            //if previous block had a decision
            if (pred.successors.size > 1) {
                var decision = BlockDecision(pred.blockDecisionVar, false)
                if (this == pred.successors.head)
                    decision = decision.negate
                if (c.pathConditions.isEmpty)
                    BlockCondition(List(BlockPathCondition(List(decision))))
                else
                    BlockCondition(c.pathConditions.map(_.add(decision)))
            } else c
        }
        conditions.fold(BlockCondition(Nil))(_ union _)
    }


}

private[instructions] case class BlockCondition(pathConditions: List[BlockPathCondition]) {

    /**
      * this implementation is inefficient, but doesn't matter for now, since it's compiletime only anyway
      *
      * it removes conditions that are obsolete
      */
    private def removeRedundancy(conditions: List[BlockPathCondition]): List[BlockPathCondition] = {
        var change = false
        var cond = conditions
        for (a <- conditions; b <- cond; if !(a eq b))
            if (a.decisions.head == b.decisions.head.negate && a.decisions.tail == b.decisions.tail) {
                cond = cond.filterNot(x => (x eq a) || (x eq b))
                if (b.decisions.tail.nonEmpty)
                    cond ::= BlockPathCondition(b.decisions.tail)
                change = true
            }
        if (change) removeRedundancy(cond) else cond
    }

    def union(d: BlockCondition) = BlockCondition(removeRedundancy(d.pathConditions ++ this.pathConditions))

    override def toString() = pathConditions.mkString(" || ")
}

private[instructions] case class BlockPathCondition(decisions: List[BlockDecision]) {
    def add(d: BlockDecision) = BlockPathCondition(d :: decisions)

    override def toString() = decisions.mkString("&&")
}

private[instructions] case class BlockDecision(idx: Int, negated: Boolean) {
    def negate = BlockDecision(idx, !negated)

    override def toString() = (if (negated) "!" else "") + idx
}


case class CFG(blocks: List[Block]) {

    blocks.foreach(_.computeSuccessors(this))
    blocks.foreach(_.computePredecessors(this))
    for (i <- 0 until blocks.size) blocks(i).idx = i

    def toByteCode(mv: MethodVisitor, method: MethodNode) = {
        //hack: unfortunately necessary, since otherwise state inside the label is shared
        //across both toByteCode and toVByteCode with leads to obscure errors inside ASM
        blocks.foreach(_.label = new Label())

        blocks.foreach(_.toByteCode(mv, method))
    }

    def toVByteCode(mv: MethodVisitor, method: MethodNode) = {
        //hack: unfortunately necessary, since otherwise state inside the label is shared
        //across both toByteCode and toVByteCode with leads to obscure errors inside ASM
        blocks.foreach(_.label = new Label())

        blocks.foreach(_.toVByteCode(mv, method))
    }
}

case class MethodNode(access: Int, name: String,
                      desc: String, signature: String, exceptions: Array[String], body: CFG) extends LiftUtils {
    val localVariables = (for (b <- body.blocks; i <- b.instr; v <- i.getVariables) yield v).toSet
    // +2 for this and ctx
    var localVariableCount: Int = if (localVariables.isEmpty) 2 else Math.max(localVariables.max, 2)
    var ctxParameter: Int = 1
    var ctxLocalVar: Int = getFreshVariable()

    def getFreshVariable(): Int = {
        localVariableCount += 1;
        localVariableCount
    }

    for (b <- body.blocks; if b.isConditionalBlock()) b.blockDecisionVar = getFreshVariable()

    def toByteCode(cw: ClassVisitor) = {
        val mv = cw.visitMethod(access, name, desc, signature, exceptions)
        mv.visitCode()
        body.toByteCode(mv, this)
        mv.visitMaxs(0, 0)
        mv.visitEnd()
    }

    def toVByteCode(cw: ClassVisitor) = {
        val mv = cw.visitMethod(access, name, liftMethodDescription(desc), signature, exceptions)
        mv.visitCode()
        body.toVByteCode(mv, this)
        mv.visitMaxs(5, 5)
        mv.visitEnd()
    }


}

case class ClassNode(name: String, members: List[MethodNode])


trait LiftUtils {
    //    val liftedPackagePrefixes = Set("edu.cmu.cs.vbc.test", "edu.cmu.cs.vbc.prog")
    val vclassname = "edu/cmu/cs/varex/V"
    val fexprclassname = "de/fosd/typechef/featureexpr/FeatureExpr"
    val vopsclassname = "edu/cmu/cs/varex/VOps"
    val vclasstype = "L" + vclassname + ";"
    val ctxParameterOffset = 1

    //    protected def shouldLift(classname: String) = liftedPackagePrefixes.exists(classname startsWith _)

    protected def liftType(t: Type): String =
        if (t == Type.VOID_TYPE) t.getDescriptor
        else
            vclasstype

    protected def liftMethodDescription(desc: String): String = {
        val mtype = Type.getMethodType(desc)
        (Type.getObjectType(fexprclassname) +: mtype.getArgumentTypes.map(liftType)).mkString("(", "", ")") + liftType(mtype.getReturnType)
    }

    def writeConstant(mv: MethodVisitor, v: Int): Unit = {
        v match {
            case 0 => mv.visitInsn(ICONST_0)
            case 1 => mv.visitInsn(ICONST_1)
            case 2 => mv.visitInsn(ICONST_2)
            case 3 => mv.visitInsn(ICONST_3)
            case 4 => mv.visitInsn(ICONST_4)
            case 5 => mv.visitInsn(ICONST_5)
            case v if v < Byte.MaxValue => mv.visitIntInsn(BIPUSH, v)
            //TODO other push operation for larger constants
        }
    }

    protected def findThisBlock(method: MethodNode): Block =
        method.body.blocks.find(_.instr.filter(_ eq this).nonEmpty).get

}