package edu.cmu.cs.vbc.vbytecode

import edu.cmu.cs.vbc.analysis.StackAnalysis
import edu.cmu.cs.vbc.vbytecode.util.LiftUtils
import org.objectweb.asm.Opcodes._
import org.objectweb.asm._
import org.objectweb.asm.tree._


case class VBCMethodNode(access: Int, name: String,
                         desc: String, signature: Option[String], exceptions: List[String], body: CFG) extends LiftUtils {

    def toByteCode(cw: ClassVisitor, clazz: VBCClassNode) = {
        val mv = cw.visitMethod(access, name, desc, signature.getOrElse(null), exceptions.toArray)
        mv.visitCode()
        body.toByteCode(mv, new MethodEnv(clazz, this))
        mv.visitMaxs(0, 0)
        mv.visitEnd()
    }

    def toVByteCode(cw: ClassVisitor, clazz: VBCClassNode) = {
        val (framesBefore, framesAfter) = doStackAnalysis(clazz)
        val mv = cw.visitMethod(access, name, liftMethodDescription(desc), liftMethodSignature(desc, signature).getOrElse(null), exceptions.toArray)
        mv.visitCode()
        body.toVByteCode(mv, new VMethodEnv(clazz, this, framesBefore, framesAfter))
        mv.visitMaxs(5, 5)
        mv.visitEnd()
    }

    def doStackAnalysis(clazz: VBCClassNode) = {
        val mn = new MethodNode(access, name, desc, signature.orNull, exceptions.toArray)
        mn.visitCode()
        body.toByteCode(mn, new MethodEnv(clazz, this))
        mn.visitMaxs(100, 100) // 100 should be enough
        mn.visitEnd()

        val stackAnalysis = new StackAnalysis(clazz.name, mn)
        (stackAnalysis.getFramesBefore, stackAnalysis.getFramesAfter)
    }

    def returnsVoid() = Type.getMethodType(desc).getReturnType == Type.VOID_TYPE

    def isMain() =
        desc == "([Ljava/lang/String;)V" && isStatic() && isPublic()

    def isStatic(): Boolean = (access & Opcodes.ACC_STATIC) > 0

    def isPublic(): Boolean = (access & Opcodes.ACC_PUBLIC) > 0

  /**
    * We need special handling for <init> method lifting.
    * JVM will complain if ALOAD 0 and INVOKESPECIAL java.lang.object.<init> is invoked
    * inside a branch, which is usually the case in our lifting (we check the ctx in the
    * beginning of each method call)
    *
    * name should be sufficient because <init> method name is special enough
    *
    * @see [[CFG.toVByteCode()]] and [[Rewrite]]
    * @return
    */
  def isInit() =
        name == "<init>"

}


/**
  * Variable, Parameter, and LocalVar are to be used in the construction
  * of the byte code
  *
  * In contrast EnvVariable, EnvParameter, and EnvLocalVar are used
  * internally to refer to specific
  */
sealed trait Variable {
    def getIdx():Option[Int] = None
}

class Parameter(val idx: Int) extends Variable {
    override def getIdx(): Option[Int] = Some(idx)
}

class LocalVar() extends Variable


case class VBCClassNode(
                           version: Int,
                           access: Int,
                           name: String,
                           signature: Option[String],
                           superName: String,
                           interfaces: List[String],
                           fields: List[VBCFieldNode],
                           methods: List[VBCMethodNode],
                           source: Option[(String, String)] = None,
                           outerClass: Option[(String, String, String)] = None,
                           visibleAnnotations: List[AnnotationNode] = Nil,
                           invisibleAnnotations: List[AnnotationNode] = Nil,
                           visibleTypeAnnotations: List[TypeAnnotationNode] = Nil,
                           invisibleTypeAnnotations: List[TypeAnnotationNode] = Nil,
                           attrs: List[Attribute] = Nil,
                           innerClasses: List[VBCInnerClassNode] = Nil
                       ) extends LiftUtils {

    def toByteCode(cv: ClassVisitor, rewriter: VBCMethodNode => VBCMethodNode = a => a) = {
        cv.visit(version, access, name, signature.getOrElse(null), superName, interfaces.toArray)
        commonToByteCode(cv)
        //        innerClasses.foreach(_.toByteCode(cv))
        fields.foreach(_.toByteCode(cv))
        methods.foreach(m => rewriter(Rewrite.rewrite(m)).toByteCode(cv, this))
        cv.visitEnd()
    }


    def toVByteCode(cv: ClassVisitor, rewriter: VBCMethodNode => VBCMethodNode = a => a) = {
        cv.visit(version, access, name, signature.getOrElse(null), superName, interfaces.toArray)
        commonToByteCode(cv)
        //        innerClasses.foreach(_.toVByteCode(cv))
        fields.foreach(_.toVByteCode(cv))
        methods.foreach(m => rewriter(Rewrite.rewriteV(m)).toVByteCode(cv, this))
        //if the class has a main method, create also an unlifted main method
        if (methods.exists(_.isMain))
            createUnliftedMain(cv)
        // Write lambda methods
        lambdaMethods.foreach(_ (cv))
        cv.visitEnd()
    }

    /**
      * we assume the main method gets lifted just as everything else;
      * here we create a new entry into the program with a normal
      * main method (without a context) that just starts the lifted
      * main method in context True
      */
    def createUnliftedMain(cv: ClassVisitor) = {
        val mainMethodSig = "([Ljava/lang/String;)V"
        val mv = cv.visitMethod(ACC_PUBLIC | ACC_STATIC, "main", mainMethodSig, mainMethodSig, Array.empty)
        mv.visitCode()
        //load array param
        mv.visitVarInsn(ALOAD, 0)
        //create a V<String[]>
        callVCreateOne(mv)
        //set context to True
        pushConstantTRUE(mv)
        mv.visitMethodInsn(INVOKESTATIC, name, "main", liftMethodDescription(mainMethodSig), false)
        mv.visitInsn(RETURN)
        mv.visitMaxs(2, 0)
        mv.visitEnd()
    }


    /**
      * parts that are not lifted
      */
    private def commonToByteCode(cv: ClassVisitor): Unit = {
        source.map(s => cv.visitSource(s._1, s._2))
        outerClass.map(s => cv.visitOuterClass(s._1, s._2, s._3))
        visibleAnnotations.foreach(a => a.accept(cv.visitAnnotation(a.desc, true)))
        invisibleAnnotations.foreach(a => a.accept(cv.visitAnnotation(a.desc, false)))
        visibleTypeAnnotations.foreach(a => a.accept(cv.visitTypeAnnotation(a.typeRef, a.typePath, a.desc, true)))
        invisibleTypeAnnotations.foreach(a => a.accept(cv.visitTypeAnnotation(a.typeRef, a.typePath, a.desc, false)))
        attrs.foreach(cv.visitAttribute)
    }

    /**
      * Generated lambdaMethods
      */
    var lambdaMethods: List[(ClassVisitor) => Unit] = Nil

}

case class VBCFieldNode(
                           access: Int,
                           name: String,
                           desc: String,
                           signature: String,
                           value: Object,

                           visibleAnnotations: List[AnnotationNode] = Nil,
                           invisibleAnnotations: List[AnnotationNode] = Nil,
                           visibleTypeAnnotations: List[TypeAnnotationNode] = Nil,
                           invisibleTypeAnnotations: List[TypeAnnotationNode] = Nil,
                           attrs: List[Attribute] = Nil
                       ) extends LiftUtils {
    def toByteCode(cv: ClassVisitor) = {
        val fv = cv.visitField(access, name, desc, signature, value)

        commonToByteCode(fv)

        fv.visitEnd()
    }

    def toVByteCode(cv: ClassVisitor) = {
        val fv = cv.visitField(access, name, vclasstype, liftPrimitiveType(desc), value)
        //TODO lift initial value for

        commonToByteCode(fv)

        fv.visitEnd()
    }

    def hasConditionalAnnotation() =
        (visibleAnnotations ++ invisibleAnnotations).exists(_.desc == "Ledu/cmu/cs/varex/annotation/VConditional;")


    private def commonToByteCode(fv: FieldVisitor): Unit = {
        visibleAnnotations.foreach(a => a.accept(fv.visitAnnotation(a.desc, true)))
        invisibleAnnotations.foreach(a => a.accept(fv.visitAnnotation(a.desc, false)))
        visibleTypeAnnotations.foreach(a => a.accept(fv.visitTypeAnnotation(a.typeRef, a.typePath, a.desc, true)))
        invisibleTypeAnnotations.foreach(a => a.accept(fv.visitTypeAnnotation(a.typeRef, a.typePath, a.desc, false)))
        attrs.foreach(fv.visitAttribute)
    }
}

case class VBCInnerClassNode()
