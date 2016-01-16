package edu.cmu.cs.vbc.instructions

import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{ClassVisitor, Label, MethodVisitor, Type}


trait Instruction extends LiftUtils {
    def toByteCode(mv: MethodVisitor, cfg: CFG)

    def toVByteCode(mv: MethodVisitor)

}

case class InstrIADD() extends Instruction {
    override def toByteCode(mv: MethodVisitor, cfg: CFG): Unit = {
        mv.visitInsn(IADD)
    }

    override def toVByteCode(mv: MethodVisitor): Unit = {
        mv.visitMethodInsn(INVOKESTATIC, vopsclassname, "IADD", "(Ledu/cmu/cs/varex/V;Ledu/cmu/cs/varex/V;)Ledu/cmu/cs/varex/V;", false)
    }
}

case class InstrICONST(v: Int) extends Instruction {
    override def toByteCode(mv: MethodVisitor, cfg: CFG): Unit = {
        writeConstant(mv, v)
    }

    override def toVByteCode(mv: MethodVisitor): Unit = {
        writeConstant(mv, v)
        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false)
        mv.visitMethodInsn(INVOKESTATIC, vclassname, "one", "(Ljava/lang/Object;)Ledu/cmu/cs/varex/V;", true)
    }
}


case class InstrRETURN() extends Instruction {
    override def toByteCode(mv: MethodVisitor, cfg: CFG): Unit =
        mv.visitInsn(RETURN)

    override def toVByteCode(mv: MethodVisitor): Unit =
        mv.visitInsn(RETURN)
}


case class InstrIINC(variable: Int, increment: Int) extends Instruction {
    override def toByteCode(mv: MethodVisitor, cfg: CFG): Unit =
        mv.visitIincInsn(variable, increment)

    override def toVByteCode(mv: MethodVisitor): Unit = {
        mv.visitVarInsn(ALOAD, variable + ctxParameterOffset)
        writeConstant(mv, increment)
        mv.visitMethodInsn(INVOKESTATIC, vopsclassname, "IINC", "(Ledu/cmu/cs/varex/V;I)Ledu/cmu/cs/varex/V;", false)
        mv.visitVarInsn(ASTORE, variable + ctxParameterOffset)
    }
}

case class InstrISTORE(variable: Int) extends Instruction {
    override def toByteCode(mv: MethodVisitor, cfg: CFG): Unit =
        mv.visitVarInsn(ISTORE, variable)

    override def toVByteCode(mv: MethodVisitor): Unit = {
        mv.visitVarInsn(ASTORE, variable + ctxParameterOffset)
    }
}

case class InstrILOAD(variable: Int) extends Instruction {
    override def toByteCode(mv: MethodVisitor, cfg: CFG): Unit =
        mv.visitVarInsn(ILOAD, variable)

    override def toVByteCode(mv: MethodVisitor): Unit = {
        mv.visitVarInsn(ALOAD, variable + ctxParameterOffset)
    }
}

case class InstrIFEQ(blockIdx: Int) extends Instruction {
    override def toByteCode(mv: MethodVisitor, cfg: CFG): Unit = {
        val targetBlock = cfg.nodes(blockIdx)
        mv.visitJumpInsn(IFEQ, targetBlock.label)
    }

    override def toVByteCode(mv: MethodVisitor): Unit = {
        //        mv.visitVarInsn(ASTORE, variable)
    }
}


case class Block(instr: Instruction*) {
    val label = new Label()

    def toByteCode(mv: MethodVisitor, cfg: CFG) = {
        mv.visitLabel(label)
        instr.foreach(_.toByteCode(mv, cfg))
    }

    def toVByteCode(mv: MethodVisitor) = {
        //        mv.visitLabel(label)
        instr.foreach(_.toVByteCode(mv))
    }

}

case class CFG(nodes: List[Block]) {

    def toByteCode(mv: MethodVisitor) = {
        nodes.foreach(_.toByteCode(mv, this))
    }

    def toVByteCode(mv: MethodVisitor) =
        nodes.foreach(_.toVByteCode(mv))
}

case class MethodNode(access: Int, name: String,
                      desc: String, signature: String, exceptions: Array[String], body: CFG) extends LiftUtils {

    def toByteCode(cw: ClassVisitor) = {
        val mv = cw.visitMethod(access, name, desc, signature, exceptions)
        mv.visitCode()
        body.toByteCode(mv)
        mv.visitMaxs(0, 0)
        mv.visitEnd()
    }

    def toVByteCode(cw: ClassVisitor) = {
        val mv = cw.visitMethod(access, name, liftMethodDescription(desc), signature, exceptions)
        mv.visitCode()
        body.toVByteCode(mv)
        mv.visitMaxs(0, 0)
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
}