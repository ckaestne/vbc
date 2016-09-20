package edu.cmu.cs.vbc.utils

import java.io.{File, FileOutputStream, FileWriter, PrintWriter}

import com.typesafe.scalalogging.LazyLogging
import edu.cmu.cs.vbc.utils.LiftUtils._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.util.TraceClassVisitor
import org.objectweb.asm.{ClassReader, ClassWriter}

/** V wrapper for classes.
  *
  * @author chupanw
  */
class VBCWrapper(fqName: String) extends LazyLogging {

  logger.info(s"Generating wrapper class for $fqName")

  val name = fqName.replace('.', '/')
  val desc = s"L$name;"

  val biFunType = "Ljava/util/function/BiFunction;"
  val biConType = "Ljava/util/function/BiConsumer;"


  def getWrapperClassBytes(): Array[Byte] = {
    val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
    createClass(name, cw)
    createField(cw)
    init(cw)
    cw.visitEnd()
    // debugging
    VBCWrapper.toFile(fqName, cw)
    cw.toByteArray
  }

  def createClass(name: String, cw: ClassWriter): Unit = {
    cw.visit(52, ACC_PUBLIC + ACC_SUPER, name, null, "java/lang/Object", null)
  }

  def createField(cw: ClassWriter): Unit = {
    val fv = cw.visitField(ACC_PUBLIC, "v", vclasstype, null, null)
    fv.visitEnd()
  }

  def init(cw: ClassWriter): Unit = {
    val mv = cw.visitMethod(ACC_PUBLIC, "<init>", s"($vclasstype)V", null, null)
    mv.visitCode()
    mv.visitVarInsn(ALOAD, 0)
    mv.visitMethodInsn(INVOKESPECIAL, "java/lang/Object", "<init>", "()V", false)
    mv.visitVarInsn(ALOAD, 0)
    mv.visitVarInsn(ALOAD, 1)
    mv.visitFieldInsn(PUTFIELD, name, "v", vclasstype)
    mv.visitInsn(RETURN)
    mv.visitMaxs(2, 2)
    mv.visitEnd()
  }
}

object VBCWrapper {

  val prefix = "wrapper"

  def toFile(name: String, cw: ClassWriter) = {
    val replaced = name.replace(".", "/")
    val file = new File("lifted/" + replaced)
    file.getParentFile.mkdirs()
    val outFile = new FileOutputStream("lifted/" + replaced + ".class")
    outFile.write(cw.toByteArray)

    val sourceOutFile = new FileWriter("lifted/" + replaced + ".txt")
    val printer = new TraceClassVisitor(new PrintWriter(sourceOutFile))
    new ClassReader(cw.toByteArray).accept(printer, 0)
  }
}
