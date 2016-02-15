package edu.cmu.cs.vbc

import java.io.PrintWriter

import edu.cmu.cs.vbc.adapter.{FieldTransformer, MethodTransformer, TreeClassAdapter, MethodAnalyzer}
import org.objectweb.asm.util.{CheckClassAdapter, TraceClassVisitor}
import org.objectweb.asm.{ClassReader, ClassVisitor, ClassWriter}

/**
  * Custom class loader to modify bytecode before loading the class.
  */
class VBCClassLoader(isLift: Boolean = false) extends ClassLoader {
  override def loadClass(name: String): Class[_] = {
    if (filterByName(name)) super.loadClass(name) else findClass(name)
  }

  override def findClass(name: String): Class[_] = {
    val cr = new ClassReader(name)
    assert(cr != null)
    val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES)  // COMPUTE_FRAMES implies COMPUTE_MAX
    cr.accept(
      getFieldTransformer(getMethodTransformer(cw)),
      0)
    val cr2 = new ClassReader(cw.toByteArray)
    cr2.accept(getCheckClassAdapter(getTraceClassVisitor(null)), 0)
    defineClass(name, cw.toByteArray, 0, cw.toByteArray.length)
  }

  /**
    * Get the default TraceClassVisitor chain, which simply prints the bytecode
    *
    * @param next next ClassVisitor in the chain, usually a ClassWriter in this case
    * @return a ClassVisitor that should be accepted by ClassReader
    */
  def getTraceClassVisitor(next: ClassVisitor): ClassVisitor = new TraceClassVisitor(next, new PrintWriter(System.out))

  def getCheckClassAdapter(next: ClassVisitor): ClassVisitor = new CheckClassAdapter(next)

  /**
    * Filter classes to modify by their names
    *
    * @param name (partial) name of the class that SHOULD be modified
    * @return false if the class needs to be modified
    */
  private def filterByName(name: String): Boolean = !name.startsWith("edu.cmu.cs.vbc.prog")

  /**
    * Transform each MethodNode into MyMethodNode
    *
    * @param next
    * @return
    */
  def getMethodTransformer(next: ClassVisitor): ClassVisitor = {
    new TreeClassAdapter(next, MethodTransformer.transformMethod, isLift)
  }

  def getFieldTransformer(next: ClassVisitor): ClassVisitor = {
    new FieldTransformer(next, isLift)
  }
}
