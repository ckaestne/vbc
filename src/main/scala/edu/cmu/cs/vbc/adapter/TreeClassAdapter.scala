package edu.cmu.cs.vbc.adapter

import org.objectweb.asm.ClassVisitor
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree.ClassNode

/**
  * My class adapter using the tree API
  */
class TreeClassAdapter(next: ClassVisitor, f: (ClassNode, Boolean) => Any, isLift: Boolean) extends ClassVisitor(ASM5, new ClassNode()) {
  override def visitEnd(): Unit = {
    val cn: ClassNode = cv.asInstanceOf[ClassNode]
    f(cn, isLift)
    cn.accept(next)
  }
}