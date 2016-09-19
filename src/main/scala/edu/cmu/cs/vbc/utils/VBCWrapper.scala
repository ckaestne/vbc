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
  val originName = VBCWrapper.getOriginalName(name)
  val desc = s"L$name;"

  val biFunType = "Ljava/util/function/BiFunction;"
  val biConType = "Ljava/util/function/BiConsumer;"


  def getWrapperClassBytes(): Array[Byte] = {
    val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
    createClass(name, cw)
    createField(cw)
    init(cw)
    //    one(cw)
    //    choice(cw)
    //    smap(cw)
    //    sforeach(cw)
    //    sflatMap(cw)
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

  def one(cw: ClassWriter): Unit = {
    val mv = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, "one", s"(${fexprclasstype}L${originName};)$desc", null, null)
    mv.visitCode()
    mv.visitTypeInsn(NEW, name)
    mv.visitInsn(DUP)
    mv.visitVarInsn(ALOAD, 0)
    mv.visitVarInsn(ALOAD, 1)
    mv.visitMethodInsn(INVOKESTATIC, vclassname, "one", s"(${fexprclasstype}Ljava/lang/Object;)$vclasstype", true)
    mv.visitMethodInsn(INVOKESPECIAL, name, "<init>", s"(${vclasstype})V", false)
    mv.visitInsn(ARETURN)
    mv.visitMaxs(4, 2)
    mv.visitEnd()
  }

  def choice(cw: ClassWriter): Unit = {
    val mv = cw.visitMethod(
      ACC_PUBLIC + ACC_STATIC,
      "choice",
      s"(${fexprclasstype}${desc}${desc})$desc",
      null,
      null
    )
    mv.visitCode()
    mv.visitTypeInsn(NEW, name)
    mv.visitInsn(DUP)
    mv.visitVarInsn(ALOAD, 0)
    mv.visitVarInsn(ALOAD, 1)
    mv.visitFieldInsn(GETFIELD, name, "v", vclasstype)
    mv.visitVarInsn(ALOAD, 2)
    mv.visitFieldInsn(GETFIELD, name, "v", vclasstype)
    mv.visitMethodInsn(INVOKESTATIC, vclassname, "choice", s"(${fexprclasstype}${vclasstype}${vclasstype})$vclasstype", true)
    mv.visitMethodInsn(INVOKESPECIAL, name, "<init>", s"($vclasstype)V", false)
    mv.visitInsn(ARETURN)
    mv.visitMaxs(5, 3)
    mv.visitEnd()
  }

  def smap(cw: ClassWriter): Unit = {
    val mv = cw.visitMethod(ACC_PUBLIC, "smap", s"(${biFunType}${fexprclasstype})$desc", null, null)
    mv.visitCode()
    mv.visitTypeInsn(NEW, name)
    mv.visitInsn(DUP)
    mv.visitVarInsn(ALOAD, 0)
    mv.visitFieldInsn(GETFIELD, name, "v", vclasstype)
    mv.visitVarInsn(ALOAD, 1)
    mv.visitVarInsn(ALOAD, 2)
    mv.visitMethodInsn(INVOKEINTERFACE, vclassname, "smap", s"(${biFunType}${fexprclasstype})$vclasstype", true)
    mv.visitMethodInsn(INVOKESPECIAL, name, "<init>", s"($vclasstype)V", false)
    mv.visitInsn(ARETURN)
    mv.visitMaxs(5, 3)
    mv.visitEnd()
  }

  def sforeach(cw: ClassWriter): Unit = {
    val mv = cw.visitMethod(ACC_PUBLIC, "sforeach", s"(${biConType}${fexprclasstype})V", null, null)
    mv.visitCode()
    mv.visitVarInsn(ALOAD, 0)
    mv.visitFieldInsn(GETFIELD, name, "v", vclasstype)
    mv.visitVarInsn(ALOAD, 1)
    mv.visitVarInsn(ALOAD, 2)
    mv.visitMethodInsn(INVOKEINTERFACE, vclassname, "sforeach", s"(${biConType}${fexprclasstype})V", true)
    mv.visitInsn(RETURN)
    mv.visitMaxs(3, 3)
    mv.visitEnd()
  }

  def sflatMap(cw: ClassWriter): Unit = {
    val mv = cw.visitMethod(ACC_PUBLIC, "sflatMap", s"(${biFunType}${fexprclasstype})$desc", null, null)
    mv.visitCode()
    mv.visitTypeInsn(NEW, name)
    mv.visitInsn(DUP)
    mv.visitVarInsn(ALOAD, 0)
    mv.visitFieldInsn(GETFIELD, name, "v", vclasstype)
    mv.visitVarInsn(ALOAD, 1)
    mv.visitVarInsn(ALOAD, 2)
    mv.visitMethodInsn(INVOKEINTERFACE, vclassname, "sflatMap", s"(${biFunType}${fexprclasstype})$vclasstype", true)
    mv.visitMethodInsn(INVOKESPECIAL, name, "<init>", s"($vclasstype)V", false)
    mv.visitInsn(ARETURN)
    mv.visitMaxs(5, 3)
    mv.visitEnd()
  }
}

object VBCWrapper {

  def getOriginalName(str: String): String = {
    val lastSlash = str.lastIndexOf('/')
    if (lastSlash != -1) {
      if (lastSlash == 1) {
        // primitive
        str.substring(2)
      } else {
        str.substring(2, lastSlash) + str.substring(lastSlash + 1)
      }
    } else {
      str.substring(1)
    }
  }

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
