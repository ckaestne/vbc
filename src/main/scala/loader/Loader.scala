package loader

import java.io.InputStream

import edu.cmu.cs.vbc.instructions.{VBCClassNode, VBCFieldNode, VBCInnerClassNode, VBCMethodNode}
import org.objectweb.asm.ClassReader
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree.{ClassNode, FieldNode, InnerClassNode, MethodNode}

import scala.collection.JavaConversions._

/**
  * My class adapter using the tree API
  */
class Loader {

    def loadClass(is: InputStream): VBCClassNode = {

        val cr = new ClassReader(is)
        val classNode = new ClassNode(ASM5)
        cr.accept(classNode, 0)

        adaptClass(classNode)
    }

    def adaptClass(cl: ClassNode): VBCClassNode = new VBCClassNode(
        cl.version,
        cl.access,
        cl.name,
        cl.signature,
        cl.superName,
        if (cl.interfaces == null) Nil else cl.interfaces.toList,
        if (cl.fields == null) Nil else cl.fields.map(adaptField).toList,
        if (cl.methods == null) Nil else cl.methods.map(adaptMethod).toList,
        if (cl.sourceDebug != null && cl.sourceFile != null) Some(cl.sourceFile, cl.sourceDebug) else None,
        if (cl.outerClass != null) Some(cl.outerClass, cl.outerMethod, cl.outerMethodDesc) else None,
        if (cl.visibleAnnotations != null) Nil else cl.visibleAnnotations.toList,
        if (cl.invisibleAnnotations != null) Nil else cl.invisibleAnnotations.toList,
        if (cl.visibleTypeAnnotations != null) Nil else cl.visibleTypeAnnotations.toList,
        if (cl.invisibleTypeAnnotations != null) Nil else cl.invisibleTypeAnnotations.toList,
        if (cl.attrs != null) Nil else cl.attrs.toList,
        if (cl.innerClasses == null) Nil else cl.innerClasses.map(adaptInnerClass).toList
    )


    def adaptMethod(m: MethodNode): VBCMethodNode = ???

    def adaptField(field: FieldNode): VBCFieldNode = new VBCFieldNode(
        field.access,
        field.name,
        field.desc,
        field.signature,
        field.value,
        if (field.visibleAnnotations != null) Nil else field.visibleAnnotations.toList,
        if (field.invisibleAnnotations != null) Nil else field.invisibleAnnotations.toList,
        if (field.visibleTypeAnnotations != null) Nil else field.visibleTypeAnnotations.toList,
        if (field.invisibleTypeAnnotations != null) Nil else field.invisibleTypeAnnotations.toList,
        if (field.attrs != null) Nil else field.attrs.toList
    )

    def adaptInnerClass(m: InnerClassNode): VBCInnerClassNode = ???

}