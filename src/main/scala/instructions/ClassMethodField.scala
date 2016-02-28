package edu.cmu.cs.vbc.instructions

import org.objectweb.asm.tree._
import org.objectweb.asm.{Attribute, ClassVisitor, FieldVisitor, Type}


case class VBCMethodNode(access: Int, name: String,
                         desc: String, signature: String, exceptions: Array[String], body: CFG) extends LiftUtils {

    def toByteCode(cw: ClassVisitor) = {
        val mv = cw.visitMethod(access, name, desc, signature, exceptions)
        mv.visitCode()
        body.toByteCode(mv, new MethodEnv(this))
        mv.visitMaxs(0, 0)
        mv.visitEnd()
    }

    def toVByteCode(cw: ClassVisitor) = {
        val mv = cw.visitMethod(access, name, liftMethodDescription(desc), signature, exceptions)
        mv.visitCode()
        body.toVByteCode(mv, new VMethodEnv(this))
        mv.visitMaxs(5, 5)
        mv.visitEnd()
    }

    def returnsVoid() = Type.getMethodType(desc).getReturnType == Type.VOID_TYPE


}


/**
  * Variable, Parameter, and LocalVar are to be used in the construction
  * of the byte code
  *
  * In contrast EnvVariable, EnvParameter, and EnvLocalVar are used
  * internally to refer to specific
  */
sealed trait Variable

class Parameter(val idx: Int) extends Variable

class LocalVar() extends Variable


case class VBCClassNode(
                           version: Int,
                           access: Int,
                           name: String,
                           signature: String,
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
                       ) {

    def toByteCode(cv: ClassVisitor) = {
        cv.visit(version, access, name, signature, superName, interfaces.toArray)
        commonToByteCode(cv)
        //        innerClasses.foreach(_.toByteCode(cv))
        //        fields.foreach(_.toByteCode(cv))
        methods.foreach(_.toByteCode(cv))
        cv.visitEnd()
    }

    def toVByteCode(cv: ClassVisitor) = {
        cv.visit(version, access, name, signature, superName, interfaces.toArray)
        commonToByteCode(cv)
        //        innerClasses.foreach(_.toByteCode(cv))
        //        fields.foreach(_.toByteCode(cv))
        methods.foreach(_.toVByteCode(cv))
        cv.visitEnd()
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
