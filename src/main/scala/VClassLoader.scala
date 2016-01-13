package edu.cmu.cs.vbc

import java.io.{PrintWriter, ByteArrayOutputStream, InputStream}
import java.lang.reflect.Method

import org.objectweb.asm._
import org.objectweb.asm.tree._
import org.objectweb.asm.util.{TraceClassVisitor, TraceMethodVisitor}
import Opcodes._


import scala.collection.JavaConversions._

/**
  * Created by ckaestne on 12/25/2015.
  */
class VClassLoader extends ClassLoader {


  val liftedPackagePrefixes = Set("edu.cmu.cs.vbc.test", "edu.cmu.cs.vbc.prog")
  val vclassname = "edu.cmu.cs.varex.V"
  val vclasstype = "L" + vclassname.replace('.', '/') + ";"

  def shouldLift(classname: String) = liftedPackagePrefixes.exists(classname startsWith _)

  def liftType(t: Type): String =
    if (t == Type.VOID_TYPE) t.getDescriptor
    else
      vclasstype

  def liftType(t: String): String =
    if (t == "V") t
    else
      vclasstype

  //      s"L$vclass<" + t.getDescriptor + ">;"

  def liftMethodDescription(desc: String): String = {
    val mtype = Type.getMethodType(desc)
    //    Type.getMethodDescriptor(mtype.getReturnType, mtype.getArgumentTypes.map(a=>a))
    mtype.getArgumentTypes.map(liftType).mkString("(", "", ")") + liftType(mtype.getReturnType)
  }

  def liftMethodSignature(method: MethodNode) = {
    method.desc = liftMethodDescription(method.desc);

    //    val mtype = Type.getMethodType(method.desc)
    //    mtype.getArgumentTypes.foreach(println(_))
    //    assert(method.parameters.forall(_.name != "ctx"), "method may not have a parameter called ctx, in " + method.toString)
    //    method.parameters.add(1, new ParameterNode("ctx", 0))
  }

  def isStatic(access: Int): Boolean = (access & Opcodes.ACC_STATIC) > 0

  def isMainMethod(x: MethodNode) = x.name == "main" && isStatic(x.access)

  def liftClass(cn: ClassNode): ClassNode = {
    for (x <- cn.methods; if !isMainMethod(x))
      liftMethodSignature(x)

    cn
  }

  def loadAndLiftClass(name: String): Class[_] = {
    println(s"loading and lifting class $name")
    val resource: String = name.replace('.', '/') + ".class"
    val is: InputStream = getResourceAsStream(resource)
    val cr = new ClassReader(is)
    //    val cn = new ClassNode()
    //    cr.accept(cn, 0)
    val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS)
    //    liftClass(cn).accept(cw)

    var lambdaClasses = List[Any]()

    val cv = new ClassVisitor(Opcodes.ASM5, new TraceClassVisitor(cw, new PrintWriter(System.out))) {
      var thisClass = ""
      var addedMethods = List[(ClassVisitor) => Unit]()
      var done = false
      // set to true after processing all existing data for generated new data
      var funGenCounter = 0


      override def visit(version: Int, access: Int, name: String, signature: String, superName: String, interfaces: Array[String]) {
        println(s"Visit: $name")
        thisClass = name
        super.visit(version, access, name, signature, superName, interfaces)
      }

      override def visitEnd(): Unit = {
        done = true
        addedMethods.foreach(_ (this))

        super.visitEnd()
      }

      def liftFieldDesc(desc: String) = liftType(desc)

      override def visitField(access: Int, name: String, desc: String, signature: String, value: Any): FieldVisitor = {
        super.visitField(access, name, liftFieldDesc(desc), liftFieldDesc(signature), value)
      }

      override def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]): MethodVisitor = {
        println(s"\tVisit-Method: $name, $desc")
        if (done || (isStatic(access) && name == "main"))
          super.visitMethod(access, name, desc, signature, exceptions)
        else
          new MethodVisitor(Opcodes.ASM5, super.visitMethod(access, name, liftMethodDescription(desc), signature, exceptions)) {

            def isTypeLifted(t: String): Boolean =
              shouldLift(t.replace('/', '.'))

            val vmapClosureType = "(Ledu/cmu/cs/varex/V;)Ljava/util/function/Function;"
            val vmapClosureName = "apply"
            val vmapCallType = "(Ljava/util/function/Function;)Ledu/cmu/cs/varex/V;"
            val vmapCallOwner = "edu/cmu/cs/varex/V"
            val vmapCallName = "vflatMap"
            val lamdaFactoryOwner = "java/lang/invoke/LambdaMetafactory"
            val lamdaFactoryMethod = "metafactory"
            val lamdaFactoryDesc = "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodType;Ljava/lang/invoke/MethodHandle;Ljava/lang/invoke/MethodType;)Ljava/lang/invoke/CallSite;"

            def liftMethodCallUsingFlatMap(opcode: Int, owner: String, name: String, desc: String, itf: Boolean): Unit = {

              //assuming that all the arguments have been pushed to the stack in a lifted
              //from. now need to call flatMap on the first argument of those by
              //creating a closure first
              val funType = Type.getMethodType(desc)

              funGenCounter += 1
              val genFunName = "vlamda$" + funGenCounter

              addedMethods ::= { case (v: ClassVisitor) => {
                val mv = v.visitMethod(Opcodes.ACC_PRIVATE | Opcodes.ACC_STATIC | Opcodes.ACC_SYNTHETIC,
                  genFunName,
                  "(Ledu/cmu/cs/varex/V;Ledu/cmu/cs/vbc/prog/I;)Ledu/cmu/cs/varex/V;",
                  "(Ledu/cmu/cs/varex/V;Ledu/cmu/cs/vbc/prog/I;)Ledu/cmu/cs/varex/V;",
                  Array[String]()
                )
                mv.visitCode()
                mv.visitVarInsn(Opcodes.ALOAD, 1)
                mv.visitVarInsn(Opcodes.ALOAD, 0)
                mv.visitMethodInsn(opcode, owner, name, liftMethodDescription(desc), itf)
                mv.visitInsn(Opcodes.ARETURN)
                mv.visitMaxs(2, 2)
                mv.visitEnd()
              }
              }

              super.visitInvokeDynamicInsn(vmapClosureName, vmapClosureType,
                new Handle(Opcodes.H_INVOKESTATIC, lamdaFactoryOwner, lamdaFactoryMethod, lamdaFactoryDesc),
                "(Ljava/lang/Object;)Ljava/lang/Object;",
                new Handle(Opcodes.H_INVOKESTATIC, thisClass, genFunName, "(Ledu/cmu/cs/varex/V;Ledu/cmu/cs/vbc/prog/VI;)Ledu/cmu/cs/varex/V;"),
                "(Ledu/cmu/cs/vbc/prog/VI;)Ledu/cmu/cs/varex/V;"
              )

              super.visitMethodInsn(Opcodes.INVOKEINTERFACE, vmapCallOwner, vmapCallName, vmapCallType, true)
              //              super.visitMethodInsn(opcode, owner, name, liftMethodDescription( desc), itf)

            }

            def liftConstructorInvocation(opcode: Int, owner: String, name: String, desc: String, itf: Boolean): Unit = {
              super.visitMethodInsn(opcode, owner, name, liftMethodDescription(desc), itf)
              call_V_one()
            }

            def call_V_one(): Unit = {
              //edu/cmu/cs/varex/V.one (Ljava/lang/Object;)Ledu/cmu/cs/varex/V;
              super.visitMethodInsn(Opcodes.INVOKESTATIC, "edu/cmu/cs/varex/V", "one", "(Ljava/lang/Object;)Ledu/cmu/cs/varex/V;", false)
            }

            val intopToLift = Set(ICONST_0, ICONST_1,
              ICONST_2, ICONST_3, ICONST_4, ICONST_5, BIPUSH)
            val opToLibcall = Set(IADD)

            def call_box_int() = {
              super.visitMethodInsn(INVOKESTATIC, "java/lang/Integer", "valueOf", "(I)Ljava/lang/Integer;", false)
            }

            def call_int_to_V() = {
              call_box_int()
              call_V_one()
            }

            def opcodeToLibCall(opcode: Int) = opcode match {
              case IADD =>
                super.visitMethodInsn(Opcodes.INVOKESTATIC, "edu/cmu/cs/vbc/VBCLib", "iadd", "(Ledu/cmu/cs/varex/V;Ledu/cmu/cs/varex/V;)Ledu/cmu/cs/varex/V;", false)
            }


            override def visitInsn(opcode: Int): Unit =
              if (opToLibcall contains opcode)
                opcodeToLibCall(opcode)
              else {
                super.visitInsn(opcode)
                if (intopToLift contains opcode)
                  call_int_to_V()
              }

            override def visitIntInsn(opcode: Int, op: Int): Unit = {
              super.visitIntInsn(opcode, op)
              if (intopToLift contains opcode)
                call_int_to_V()
            }

            override def visitVarInsn(opcode: Int, v: Int): Unit = {
              if (opcode == ILOAD)
                super.visitVarInsn(ALOAD, v)
              else
                super.visitVarInsn(opcode, v)
            }

            override def visitMethodInsn(opcode: Int, owner: String, name: String, desc: String, itf: Boolean): Unit = {
              if (!isTypeLifted(owner))
                return super.visitMethodInsn(opcode, owner, name, desc, itf)
              println(s"\t\tVisit-Call: $owner->$name, $desc")
              //no need to lift static calls (nonvariational receiver)
              //no need to lift this.*() calls (nonvariational receiver)
              if (owner == thisClass || opcode == Opcodes.INVOKESTATIC)
                return super.visitMethodInsn(opcode, owner, name, liftMethodDescription(desc), itf)
              else if (opcode == Opcodes.INVOKESPECIAL) //lift constructor calls
                liftConstructorInvocation(opcode, owner, name, desc, itf)
              else //call on lifted (variational) value lifted through flatMap
                liftMethodCallUsingFlatMap(opcode, owner, name, desc, itf)
            }

            override def visitFieldInsn(opcode: Int, owner: String, name: String,
                                        desc: String) =
              if (!isTypeLifted(owner))
                super.visitFieldInsn(opcode, owner, name, desc)
              else
                super.visitFieldInsn(opcode, owner, name, liftFieldDesc(desc))

            override def visitLocalVariable(name: String, desc: String, signature: String,
                                            start: Label, end: Label, index: Int): Unit = {
              if (name == "this")
                super.visitLocalVariable(name, desc, signature, start, end, index)
              else
                super.visitLocalVariable(name, liftType(desc), signature, start, end, index)
            }
          }
      }

    }
    cr.accept(cv, 0)

    val classByte: Array[Byte] = cw.toByteArray
    return defineClass(name, classByte, 0, classByte.length, null)
  }


  override def loadClass(name: String, resolve: Boolean): Class[_] = {
    if (shouldLift(name)) {
      loadAndLiftClass(name)
    } else
      super.loadClass(name, resolve)

  }

}

object VLauncher extends App {

  val loader: ClassLoader = new VClassLoader()
  val c: Class[_] = loader.loadClass(args(0))
  val m: Method = c.getMethod("main", Array[String]().getClass)
  m.invoke(null, args.drop(1))
}
