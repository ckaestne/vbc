package edu.cmu.cs.vbc.instructions

import org.objectweb.asm.Opcodes._
import org.objectweb.asm.{ClassVisitor, MethodVisitor, Type}


trait Instruction extends LiftUtils {
    def toByteCode(mv: MethodVisitor)

    def toVByteCode(mv: MethodVisitor)

}

case class InstrIADD() extends Instruction {
    override def toByteCode(mv: MethodVisitor): Unit = {
        mv.visitInsn(IADD)
    }

    override def toVByteCode(mv: MethodVisitor): Unit = {
        mv.visitMethodInsn(INVOKESTATIC, vopsclassname, "IADD", "(Ledu/cmu/cs/varex/V;Ledu/cmu/cs/varex/V;)Ledu/cmu/cs/varex/V;", false)
    }
}

//case class InstrILOAD() extends Instruction
//case class InstrISTORE() extends Instruction
case class InstrICONST() extends Instruction {
    override def toByteCode(mv: MethodVisitor): Unit = {
        mv.visitInsn(ICONST_0)
    }

    override def toVByteCode(mv: MethodVisitor): Unit = {
        mv.visitInsn(ICONST_0)
        mv.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false)
        mv.visitMethodInsn(INVOKESTATIC, vclassname, "one", "(Ljava/lang/Object;)Ledu/cmu/cs/varex/V;", true)
    }
}


case class InstrRETURN() extends Instruction {
    override def toByteCode(mv: MethodVisitor): Unit =
        mv.visitInsn(RETURN)

    override def toVByteCode(mv: MethodVisitor): Unit = toByteCode(mv)
}

//case class InstrINVOKESTATIC() extends Instruction
//case class InstrINVOKEVIRTUAL() extends Instruction


case class Block(instr: List[Instruction]) {
    def toByteCode(mv: MethodVisitor) = {
        instr.foreach(_.toByteCode(mv))
    }

    def toVByteCode(mv: MethodVisitor) = {
        instr.foreach(_.toVByteCode(mv))
    }
}

case class CFG(nodes: List[Block]) {

    def toByteCode(mv: MethodVisitor) = {
        nodes.foreach(_.toByteCode(mv))
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
    val vopsclassname = "edu/cmu/cs/varex/VOps"
    val vclasstype = "L" + vclassname + ";"

    //    protected def shouldLift(classname: String) = liftedPackagePrefixes.exists(classname startsWith _)

    protected def liftType(t: Type): String =
        if (t == Type.VOID_TYPE) t.getDescriptor
        else
            vclasstype

    protected def liftMethodDescription(desc: String): String = {
        val mtype = Type.getMethodType(desc)
        mtype.getArgumentTypes.map(liftType).mkString("(", "", ")") + liftType(mtype.getReturnType)
    }
}