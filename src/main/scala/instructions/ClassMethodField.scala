package edu.cmu.cs.vbc.instructions

import org.objectweb.asm.ClassVisitor


case class MethodNode(access: Int, name: String,
                      desc: String, signature: String, exceptions: Array[String], body: CFG) extends LiftUtils {

    def toByteCode(cw: ClassVisitor) = {
        val mv = cw.visitMethod(access, name, desc, signature, exceptions)
        mv.visitCode()
        body.toByteCode(mv, new MethodEnv(this))
        mv.visitMaxs(0, 0)
        mv.visitEnd()
    }

    def toVByteCode(cw: ClassVisitor) = {
        val mv = cw.visitMethod(access, name, liftMethodDescription(desc), signature, exceptions)
        mv.visitCode()
        body.toVByteCode(mv, new MethodEnv(this))
        mv.visitMaxs(5, 5)
        mv.visitEnd()
    }


}


/**
  * Variable, Parameter, and LocalVar are to be used in the construction
  * of the byte code
  *
  * In contrast EnvVariable, EnvParameter, and EnvLocalVar are used
  * internally to refer to specific
  */
sealed trait Variable

class Parameter(val idx: Int) extends Variable

class LocalVar() extends Variable


case class ClassNode(name: String, members: List[MethodNode])
