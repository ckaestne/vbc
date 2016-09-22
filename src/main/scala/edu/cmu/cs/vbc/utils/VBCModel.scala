package edu.cmu.cs.vbc.utils

import java.io._

import com.typesafe.scalalogging.LazyLogging
import edu.cmu.cs.vbc.vbytecode.{MethodDesc, Owner, TypeDesc}
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree._
import org.objectweb.asm.util.TraceClassVisitor
import org.objectweb.asm.{ClassReader, ClassWriter}

import scala.collection.JavaConversions._

/** Create model class for JDK classes
  *
  * Copy existing code from JDK, and then replace all JDK classes with model classes
  *
  * @param fqName
  * fully qualified name of this class
  * @author chupanw
  */
class VBCModel(fqName: String) extends LazyLogging {

  require(fqName.startsWith(VBCModel.prefix), "Cannot create model class for: " + fqName)
  logger.info(s"Generating model class: " + fqName)

  val modelClsName: String = fqName.replace('.', '/')
  val originalClsName: String = modelClsName.substring(VBCModel.prefix.length + 1)

  def getModelClassBytes(): Array[Byte] = {
    val resource: String = originalClsName + ".class"
    val is: InputStream = ClassLoader.getSystemResourceAsStream(resource)
    assert(is != null, s"class $originalClsName not found")
    val cr = new ClassReader(is)
    val cn = new ClassNode(ASM5)
    cr.accept(cn, 0)
    transformClass(cn)
    val cw = new MyClassWriter(ClassWriter.COMPUTE_FRAMES)
    cn.accept(cw)
    // debugging
    VBCModel.toFile(fqName, cw)
    cw.toByteArray
  }

  def rename(n: String): String = Owner(n).toModel

  def rewriteTypeDesc(t: String): String = TypeDesc(t).toModel

  def rewriteMethodDesc(m: String): String = MethodDesc(m).toModels

  def transformClass(cn: ClassNode): Unit = {
    cn.interfaces.foreach(rename)
    cn.name = rename(cn.name)
    cn.sourceFile = null
    cn.superName = rename(cn.superName)
    // outer class, outer method
    if (cn.outerClass != null) cn.outerClass = rename(cn.outerClass)
    if (cn.outerMethodDesc != null) cn.outerMethodDesc = MethodDesc(cn.outerMethodDesc).toModels
    // fields
    cn.fields.foreach(transformField)
    // methods
    cn.methods.foreach(transformMethod)
    // inner classes
    cn.innerClasses.foreach(transformInnerClass)
  }

  def transformInnerClass(in: InnerClassNode): Unit = {
    in.name = rename(in.name)
    if (in.outerName != null) in.outerName = rename(in.outerName)
  }

  def transformField(f: FieldNode): Unit = {
    f.desc = rewriteTypeDesc(f.desc)
  }

  def transformMethod(m: MethodNode): Unit = {
    m.desc = rewriteMethodDesc(m.desc)
    m.exceptions.foreach(rename)
    if (m.localVariables != null) m.localVariables.foreach(lv => lv.desc = rewriteTypeDesc(lv.desc))
    m.instructions.toArray.foreach(transformInsn)
  }

  def transformInsn(i: AbstractInsnNode): Unit = {
    i match {
      case fi: FieldInsnNode =>
        fi.owner = rename(fi.owner)
        fi.desc = rewriteTypeDesc(fi.desc)
      case d: InvokeDynamicInsnNode => ??? // not sure
      case m: MethodInsnNode =>
        m.owner = rename(m.owner)
        m.desc = rewriteMethodDesc(m.desc)
      case ma: MultiANewArrayInsnNode =>
        ma.desc = rename(ma.desc)
      case t: TypeInsnNode =>
        try {
          t.desc = rename(t.desc)
        } catch {
          case e: IllegalArgumentException => t.desc = rewriteTypeDesc(t.desc)
        }
      case _ => // keep it as it is
    }
  }
}

object VBCModel {

  val prefix = "model"

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

class MyClassWriter(flag: Int) extends ClassWriter(flag) {
  override protected def getCommonSuperClass(type1: String, type2: String): String = {
    val t1 = if (type1.startsWith(VBCModel.prefix)) type1.substring(VBCModel.prefix.length + 1) else type1
    val t2 = if (type2.startsWith(VBCModel.prefix)) type2.substring(VBCModel.prefix.length + 1) else type2
    super.getCommonSuperClass(t1, t2)
  }
}
