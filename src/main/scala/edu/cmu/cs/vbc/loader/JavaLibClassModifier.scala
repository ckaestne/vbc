package edu.cmu.cs.vbc.loader

import org.objectweb.asm.{Attribute, ClassVisitor}
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree.ClassNode

/**
  * @author chupanw
  */
class JavaLibClassModifier(next: ClassVisitor) extends ClassNode(ASM5) {

  override def visitEnd(): Unit = {
    // Remove final from Java library classes
    access = access & ~ACC_FINAL
    accept(next)
  }
}
