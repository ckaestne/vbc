package edu.cmu.cs.vbc.utils

import java.io.PrintWriter

import edu.cmu.cs.vbc.adapter.{FieldTransformer, MethodTransformer, TreeClassAdapter}
import org.objectweb.asm.util.{TraceClassVisitor, CheckClassAdapter}
import org.objectweb.asm.{ClassVisitor, ClassWriter, ClassReader}

//TODO: Move transform code to here
object ClassTransformer extends App{

  val cr = new ClassReader(args(0))
  val isLift = args(1) == "true"
  val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES)  // COMPUTE_FRAMES implies COMPUTE_MAX
  cr.accept(
    getFieldTransformer(getMethodTransformer(cw)),
    0)
  val cr2 = new ClassReader(cw.toByteArray)
  cr2.accept(new CheckClassAdapter(new TraceClassVisitor(null, new PrintWriter(System.out))), 0)

  def getMethodTransformer(next: ClassVisitor): ClassVisitor = {
    new TreeClassAdapter(next, MethodTransformer.transformMethod, isLift)
  }

  def getFieldTransformer(next: ClassVisitor): ClassVisitor = {
    new FieldTransformer(next, isLift)
  }
}
