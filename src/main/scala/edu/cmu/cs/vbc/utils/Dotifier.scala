package edu.cmu.cs.vbc.utils

import org.objectweb.asm.tree.{ClassNode, MethodNode}
import org.objectweb.asm.util.Printer
import org.objectweb.asm.util.Printer.{OPCODES, TYPES}
import org.objectweb.asm._

import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * A printer that prints dot files of classes it visits
  *
  * @author chupanw
  */
class Dotifier(clazz: ClassNode) extends Printer(Opcodes.ASM5) {

  val textBuf: ListBuffer[String] = ListBuffer.empty[String]
  val edges: ListBuffer[(String, Label)] = ListBuffer.empty
  val tryCatchEdges: ListBuffer[(Label, Label, Label, String)] = ListBuffer.empty
  val label2Node: mutable.Map[Label, String] = mutable.Map.empty[Label, String]
  val tab1 = "    "
  val tab2 = "        "
  val tab3 = "            "
  var currentBlock: Int = 0
  var currentMethod: MethodNode = _

  override def visit(version: Int,
                     access: Int,
                     name: String,
                     signature: String,
                     superName: String,
                     interfaces: Array[String]) = {
    textBuf += s"digraph $name {\n"
  }

  override def visitSource(file: String, debug: String) = {}

  override def visitOuterClass(owner: String, name: String, desc: String) = {}

  override def visitClassAnnotation(desc: String, visible: Boolean): Dotifier = new Dotifier(clazz)

  override def visitClassAttribute(attr: Attribute) = {}

  override def visitInnerClass(name: String, outerName: String, innerName: String, access: Int) = {}

  override def visitField(access: Int, name: String, desc: String, signature: String, value: scala.Any) = new Dotifier(clazz)

  override def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]) = {
    textBuf += tab1 + "subgraph " + name + s"($desc)" + " {\n" + tab2 + s"b$currentBlock [shape=box, label=" + '\"'
    currentMethod = clazz.methods.toList.find(m => m.name == name && m.desc == desc).get
    new Dotifier(clazz)
  }

  override def visitClassEnd() = textBuf += "}"

  override def visit(name: String, value: scala.Any) = {}

  override def visitEnum(name: String, desc: String, value: String) = {}

  override def visitAnnotation(name: String, desc: String) = new Dotifier(clazz)

  override def visitArray(name: String) = new Dotifier(clazz)

  override def visitAnnotationEnd() = {}

  override def visitFieldAnnotation(desc: String, visible: Boolean) = new Dotifier(clazz)

  override def visitFieldAttribute(attr: Attribute) = {}

  override def visitFieldEnd() = {}

  override def visitAnnotationDefault() = new Dotifier(clazz)

  override def visitMethodAnnotation(desc: String, visible: Boolean) = new Dotifier(clazz)

  override def visitParameterAnnotation(parameter: Int, desc: String, visible: Boolean) = new Dotifier(clazz)

  override def visitMethodAttribute(attr: Attribute) = {}

  override def visitCode() = {}

  override def visitFrame(`type`: Int, nLocal: Int, local: Array[AnyRef], nStack: Int, stack: Array[AnyRef]) = {}

  override def visitInsn(opcode: Int) = {
    textBuf += OPCODES(opcode) + "\\n"
  }

  override def visitIntInsn(opcode: Int, operand: Int) = {
    val buf = new StringBuilder
    buf.append(OPCODES(opcode))
      .append(' ')
      .append(if (opcode == Opcodes.NEWARRAY) TYPES(operand) else operand)
      .append("\\n")
    textBuf += buf.toString()
  }

  /**
    * @todo Add type information
    */
  override def visitVarInsn(opcode: Int, `var`: Int) = {
    textBuf += OPCODES(opcode) + " " + `var` + s"// ${currentMethod.localVariables.toList(`var`).desc}" + "\\n"
  }

  override def visitTypeInsn(opcode: Int, `type`: String) = {
    textBuf += OPCODES(opcode) + " " + `type` + "\\n"
  }

  override def visitFieldInsn(opcode: Int, owner: String, name: String, desc: String) = {
    textBuf += OPCODES(opcode) + " " + s"$owner.$name : $desc\\n"
  }

  override def visitInvokeDynamicInsn(name: String, desc: String, bsm: Handle, bsmArgs: AnyRef*) = {
    val buf = new StringBuilder
    buf.append("INVOKEDYNAMIC " + name + desc + "[\\n")
    //todo: shorten for efficient graph representation, but we might want to see more information
    textBuf += buf.toString()
  }

  override def visitMethodInsn(opcode: Int, owner: String, name: String, desc: String, itf: Boolean) = {
    textBuf += OPCODES(opcode) + " " + s"$owner.$name $desc\\n"
  }

  override def visitJumpInsn(opcode: Int, label: Label) = {
    val buf = new StringBuilder
    // finish the current node
    buf ++= "\"]\n"
    // start the next node
    buf ++= tab2 + s"b${currentBlock + 1} [shape=box, label=" + '\"'
    edges.append((s"b$currentBlock", label))
    currentBlock += 1
  }

  override def visitLabel(label: Label) = {
    // due to redundant labels, there might be multiple labels inside one block, but
    // one of them must be the start of the block
    label2Node += (label -> s"b$currentBlock")
  }

  override def visitLdcInsn(cst: scala.Any) = {
    val cstString = cst match {
      case s: String => s
      case t: Type => t.getDescriptor + ".class"
      case _ => cst.toString
    }
    textBuf += "LDC " + cstString + "\\n"
  }

  override def visitIincInsn(`var`: Int, increment: Int) = {
    textBuf += "IINC " + `var` + " " + increment + "\\n"
  }

  override def visitTableSwitchInsn(min: Int, max: Int, dflt: Label, labels: Label*) = ???  // should not happen

  override def visitLookupSwitchInsn(dflt: Label, keys: Array[Int], labels: Array[Label]) = ??? // should not happen

  override def visitMultiANewArrayInsn(desc: String, dims: Int) = {
    textBuf += "MULTIANEWARRAY " + desc + " " + dims + "\\n"
  }

  override def visitTryCatchBlock(start: Label, end: Label, handler: Label, `type`: String) = {
    tryCatchEdges.append((start, end, handler, `type`))
  }

  override def visitLocalVariable(name: String, desc: String, signature: String, start: Label, end: Label, index: Int) = {}

  override def visitLineNumber(line: Int, start: Label) = {
    textBuf += tab2 + "LINENUMBER " + line + " " + start + "\\n"
  }

  override def visitMaxs(maxStack: Int, maxLocals: Int) = {}

  override def visitMethodEnd() = {
    val buf = new mutable.StringBuilder()
    for ((start: String, end: Label) <- edges) {
      buf.append(tab2).append(start).append(" -> ").append(label2Node(end)).append("\n")
    }
    for ((start: Label, end: Label, handler: Label, exp: String) <- tryCatchEdges) {
      val startID: Int = label2Node(start).tail.toInt
      val endID: Int = label2Node(end).tail.toInt
      val handlerBlock: String = label2Node(handler)
      for (i <- Range.inclusive(startID, endID)) {
        buf.append(tab2).append(s"b$i -> $handlerBlock [style=dashed, taillabel=$exp]\n")
      }
    }
    buf ++= tab1 + "}"
    textBuf += buf.toString()
  }
}
