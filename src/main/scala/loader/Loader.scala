package loader

import java.io.InputStream

import edu.cmu.cs.vbc.instructions.{VBCClassNode, VBCMethodNode}
import org.objectweb.asm.ClassReader
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree.{ClassNode, MethodNode}

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

    def adaptClass(cl: ClassNode): VBCClassNode = ???

    //    {
    //        new VBCClassNode(cl.name, cl.methods.toList.map(loadMethod))
    //    }

    def loadMethod(m: MethodNode): VBCMethodNode = ???

}