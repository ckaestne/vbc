package edu.cmu.cs.vbc.instructions

import org.objectweb.asm.tree.MethodNode
import org.objectweb.asm.{ClassVisitor, Type, Opcodes}


case class MyMethodNode(myAccess: Int,
                        myName: String,
                        myDesc: String, // we need `var` because desc will get modified if isLift is true
                        mySignature: String,
                        myExceptions: Array[String],
                        body: CFG,
                        isLift: Boolean = false) extends MethodNode(Opcodes.ASM5, myAccess, myName, myDesc, mySignature, myExceptions) with LiftUtils {

  // TODO: filter here
  if (isLift && myName != "<init>" && myName != "main") {
    desc = liftMethodDescription(myDesc)
  }

  def toByteCode(cw: ClassVisitor) = {
    val mv = cw.visitMethod(myAccess, myName, myDesc, mySignature, myExceptions)
    mv.visitCode()
    body.toByteCode(mv, new MethodEnv(this))
    mv.visitMaxs(0, 0)
    mv.visitEnd()
  }

  def toVByteCode(cw: ClassVisitor) = {
    val mv = cw.visitMethod(myAccess, myName, liftMethodDescription(myDesc), mySignature, myExceptions)
    mv.visitCode()
    body.toVByteCode(mv, new VMethodEnv(this))
    mv.visitMaxs(5, 5)
    mv.visitEnd()
  }

  def transform(env: MethodEnv) = {
    this.visitCode()
    if (isLift) {
      assert(env.isInstanceOf[VMethodEnv])
      body.toVByteCode(this, env.asInstanceOf[VMethodEnv])
    }
    else
      body.toByteCode(this, env)
    this.visitMaxs(0, 0)
    this.visitEnd()
  }

  def returnsVoid() = Type.getMethodType(myDesc).getReturnType == Type.VOID_TYPE
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


case class ClassNode(name: String, members: List[MyMethodNode])
