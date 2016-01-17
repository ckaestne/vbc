package edu.cmu.cs.vbc.instructions

import org.objectweb.asm.ClassVisitor


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
