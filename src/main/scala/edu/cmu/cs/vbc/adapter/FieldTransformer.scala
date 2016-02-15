package edu.cmu.cs.vbc.adapter

import org.objectweb.asm.tree.ClassNode
import org.objectweb.asm.{FieldVisitor, AnnotationVisitor, ClassVisitor}
import org.objectweb.asm.Opcodes._
import scala.collection.mutable.Set

/**
  * @author chupanw
  */
class FieldTransformer(next: ClassVisitor, isLift: Boolean = false) extends ClassVisitor(ASM5) {

  val transformField = (cn: ClassNode, isLift: Boolean) => {
    for (i <- 0 until cn.fields.size() if isLift) {
      val field = cn.fields.get(i)
      field.signature = sig(field.desc)
      field.desc = "Ledu/cmu/cs/varex/V;"
    }
  }

  def sig(desc: String): String = {
    desc match {
      case "Z" => V("Ljava/lang/Boolean;")
      case "C" => V("Ljava/lang/Char;")
      case "B" => V("Ljava/lang/Byte;")
      case "S" => V("Ljava/lang/Short;")
      case "I" => V("Ljava/lang/Integer;")
      case "F" => V("Ljava/lang/Float;")
      case "J" => V("Ljava/lang/Long;")
      case "D" => V("Ljava/lang/Double")
      case _ => V(desc)
    }
  }

  def V(s: String) = "Ledu/cmu/cs/varex/V<" + s + ">;"

  cv = new TreeClassAdapter(next, transformField, isLift)


  override def visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]): Unit = {
    FieldTransformer.fields.clear()
    super.visit(version, access, name, signature, superName, interfaces)
  }

  override def visitField(access: Int, name: String, desc: String, signature: String, value: scala.Any): FieldVisitor = {
    val mv = super.visitField(access, name, desc, signature, value)
    if (mv != null) {
      new FieldAnnotationScanner(name, mv)
    }
    else {
      mv
    }
  }
}


class FieldAnnotationScanner(name: String, next: FieldVisitor) extends FieldVisitor(ASM5, next) {


  override def visitAnnotation(desc: String, visible: Boolean): AnnotationVisitor = {
    if (desc == "Ledu/cmu/cs/varex/annotation/VConditional;") {
      FieldTransformer.fields += name
      println("Conditional Field Identified: " + name)
    }
    super.visitAnnotation(desc, visible)
  }
}


object FieldTransformer {
  val fields: Set[String] = Set()
}