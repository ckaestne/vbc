import java.io.PrintWriter
import java.lang.reflect.InvocationTargetException

import edu.cmu.cs.vbc.vbytecode._
import edu.cmu.cs.vbc.vbytecode.instructions.{InstrALOAD, InstrINVOKESPECIAL, InstrRETURNVoid}
import org.objectweb.asm.Opcodes._
import org.objectweb.asm._
import org.objectweb.asm.util.TraceClassVisitor
import org.scalatest.FunSuite


/**
  * Created by ckaestne on 4/4/2016.
  */
class TestPlain extends FunSuite {

  private class MyClassLoader extends ClassLoader(this.getClass.getClassLoader) {
    def defineClass(name: String, b: Array[Byte]): Class[_] = {
      return defineClass(name, b, 0, b.length)
    }
  }


  def testMethod(mv: (MethodVisitor) => Unit): Unit = {

    val clazz = loadTestClass(mv)
    val obj = clazz.newInstance()
    clazz.getMethod("test").invoke(obj)
  }


  def loadTestClass(methodGen: (MethodVisitor) => Unit): Class[_] = {
    val cv = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS)
    //    clazz.toByteCode(cw)

    val constr = new VBCMethodNode(ACC_PUBLIC, "<init>", "()V", Some("()V"), Nil,
      CFG(List(
        Block(InstrALOAD(new Parameter(0, "this")),
          InstrINVOKESPECIAL("java/lang/Object", "<init>", "()V", false)),
        Block(InstrRETURNVoid())
      )))
    val testMethod = new VBCMethodNode(ACC_PUBLIC, "test", "()V", None, Nil, CFG(Nil)) {
      override def toByteCode(cw: ClassVisitor, clazz: VBCClassNode) {
        val mv = cw.visitMethod(access, name, desc, signature.orNull, exceptions.toArray)
        mv.visitCode()
        methodGen(mv)
        mv.visitMaxs(0, 0)
        mv.visitEnd()
      }
    }
    val classNode = new VBCClassNode(V1_8, ACC_PUBLIC, "Test", None, "java/lang/Object", Nil, Nil,
      List(constr, testMethod))
    classNode.toByteCode(cv)

    val byte = cv.toByteArray
    val printer = new TraceClassVisitor(new PrintWriter(System.out))
    new ClassReader(byte).accept(printer, 0)
    val myClassLoader = new MyClassLoader
    myClassLoader.defineClass("Test", byte)

  }

  test("check infrastructure") {
    testMethod((mv) => {
      mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
      mv.visitInsn(ICONST_2)
      mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(I)V", false)
      mv.visitInsn(RETURN)
    })
  }

  test("method call with element on stack") {
    testMethod((mv) => {
      mv.visitInsn(ICONST_3)
      mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
      mv.visitInsn(ICONST_2)
      mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(I)V", false)
      mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
      mv.visitInsn(SWAP)
      mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(I)V", false)
      mv.visitInsn(RETURN)
    })
  }


  test("exception") {
    intercept[InvocationTargetException] {
      testMethod((mv) => {
        mv.visitInsn(ICONST_3)
        mv.visitInsn(ICONST_0)
        mv.visitInsn(IDIV)
        mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
        mv.visitInsn(ICONST_2)
        mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(I)V", false)
        mv.visitInsn(RETURN)
      })
    }
  }

  test("catching exception") {
    testMethod((mv) => {
      val L1 = new Label()
      val L2 = new Label()
      val LEx = new Label()

      mv.visitInsn(ICONST_3)
      mv.visitInsn(ICONST_0)
      mv.visitLabel(L1)
      mv.visitInsn(IDIV)
      mv.visitLabel(L2)
      mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
      mv.visitInsn(SWAP)
      mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(I)V", false)
      mv.visitInsn(RETURN)
      mv.visitLabel(LEx)
      mv.visitInsn(POP)
      mv.visitInsn(ICONST_4)
      mv.visitJumpInsn(GOTO, L2)
      mv.visitTryCatchBlock(L1, L2, LEx, "java/lang/ArithmeticException")
    })
  }

  test("catching exception with extra stack entry") {
    testMethod((mv) => {
      val L1 = new Label()
      val L2 = new Label()
      val LEx = new Label()

      mv.visitInsn(ICONST_5)
      mv.visitInsn(ICONST_3)
      mv.visitInsn(ICONST_0)
      mv.visitLabel(L1)
      mv.visitInsn(IDIV)
      mv.visitLabel(L2)
      mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
      mv.visitInsn(SWAP)
      mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(I)V", false)
      mv.visitFieldInsn(GETSTATIC, "java/lang/System", "out", "Ljava/io/PrintStream;")
      mv.visitInsn(SWAP)
      mv.visitMethodInsn(INVOKEVIRTUAL, "java/io/PrintStream", "println", "(I)V", false)
      mv.visitInsn(RETURN)
      mv.visitLabel(LEx)
      mv.visitInsn(ICONST_4)
      mv.visitJumpInsn(GOTO, L2)
      mv.visitTryCatchBlock(L1, L2, LEx, "java/lang/ArithmeticException")
    })
  }
}
