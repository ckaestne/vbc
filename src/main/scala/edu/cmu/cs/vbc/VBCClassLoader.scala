package edu.cmu.cs.vbc

import java.io._

import com.typesafe.scalalogging.LazyLogging
import edu.cmu.cs.vbc.loader.Loader
import edu.cmu.cs.vbc.utils.Dotifier
import edu.cmu.cs.vbc.vbytecode.{VBCClassNode, VBCMethodNode}
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree.ClassNode
import org.objectweb.asm.util.{CheckClassAdapter, Textifier, TraceClassVisitor}
import org.objectweb.asm.{ClassReader, ClassVisitor, ClassWriter}

import scala.collection.mutable
import scala.sys.process.Process

object Lifting {
  val liftedPrefix = "Δ"

  def liftNameSlash(n: String) = liftedPrefix + "/" + n

  def liftNameDot(n: String) = liftedPrefix + "." + n

  def unliftName(n: String) = n.drop(2)

  def isLiftedName(n: String) = n startsWith liftedPrefix
}

/**
  * Custom class loader to modify bytecode before loading the class.
  *
  * the @param rewriter parameter allows the user of the classloader to
  * perform additional instrumentation as a last step before calling
  * toByteCode/toVByteCode when the class is loaded;
  * by default no rewriting is performed
  */
class VBCClassLoader(parentClassLoader: ClassLoader,
                     isLift: Boolean = true,
                     rewriter: (VBCMethodNode, VBCClassNode) => VBCMethodNode = (a, b) => a,
                     config: (String) => Option[Boolean] = (c) => None,
                     toFileDebugging: Boolean = true,
                     liftingPolicy: LiftingPolicy2 = NoLiftPolicy,
                     configFile: Option[String] = None) extends ClassLoader(parentClassLoader) with LazyLogging {

  import Lifting._

  val loader = new Loader()
  //TODO rather pass in the actual configuration object, not a path. this way different client can
  //configure it in different ways (only an interface needed), rather than having to create a file in the
  //specific format used here
  //  if (configFile.isDefined) {
  //    val config = new ModelConfig(configFile.get)
  //    LiftingPolicy.setConfig(config)
  //  }

  val loadedClasses = mutable.Map[String, Class[_]]()

  def stripLiftedPrefix(n: String): String =
    if (n startsWith "model.") stripLiftedPrefix(n.drop(6))
    else if (isLiftedName(n)) stripLiftedPrefix(unliftName(n))
    else n

  override def loadClass(name: String): Class[_] = loadedClasses.getOrElseUpdate(name, {
    println(s":: loading $name")

    if (isLiftedName(name))
      findClass(name) //load and lift
    else
      super.loadClass(name) // load without lifting
  })


  override def findClass(name: String): Class[_] = {
    assert(isLiftedName(name))
    val resource: String = unliftName(name).replace('.', '/') + ".class"
    val is: InputStream = getResourceAsStream(resource)
    //    assert(is != null, s"Class file not found: $name")
    if (is == null) throw new ClassNotFoundException(name)
    val clazz: VBCClassNode = loader.loadClass(is)
    liftClass(name, clazz)
  }

  //  def loadClassWithoutChanges(name: String): Class[_] = {
  //    val resource: String = name.replace('.', '/') + ".class"
  //    val is: InputStream = getResourceAsStream(resource)
  //    if (is == null) throw new ClassNotFoundException(name)
  //    val cr = new ClassReader(is)
  //    val cw = new MyClassWriter(ClassWriter.COMPUTE_FRAMES) // COMPUTE_FRAMES implies COMPUTE_MAX
  //    cr.accept(cw, 0)
  //    defineClass(name, cw.toByteArray, 0, cw.toByteArray.length)
  //  }
  //
  //  def loadClassAndUseModelClasses(name: String): Class[_] = {
  //    val resource: String = name.replace('.', '/') + ".class"
  //    val is: InputStream = getResourceAsStream(resource)
  //    val clazz: VBCClassNode = loader.loadClass(is)
  //    val cw = new MyClassWriter(ClassWriter.COMPUTE_FRAMES) // COMPUTE_FRAMES implies COMPUTE_MAX
  //    clazz.toByteCode(cw, rewriter, config)
  //    if (toFileDebugging)
  //      toFile(name, cw)
  //    defineClass(name, cw.toByteArray, 0, cw.toByteArray.length)
  //  }

  def liftClass(name: String, clazz: VBCClassNode): Class[_] = {
    println(s":: creating $name")
    import scala.collection.JavaConversions._
    val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES) // COMPUTE_FRAMES implies COMPUTE_MAX
    val dotifier = new Dotifier()
    val textifier = new Textifier()
    val cv = new TraceClassVisitor(new TraceClassVisitor(cw, textifier, null), dotifier, null)
    try {
      if (isLift) {
        logger.info(s"lifting $name")
        clazz.toVByteCode(cv, rewriter, config, liftingPolicy)
      }
      else {
        logger.info(s"lifting $name")
        clazz.toByteCode(cv, rewriter, config, liftingPolicy)
      }
    } catch {
      case e: Throwable =>
        println("Exception thrown in ASM: ")
        println(e.getClass + ": " + e.getMessage)
        println(e.getStackTrace.toList mkString("\t", "\n\t", "\n"))
        println("Please check bug.gv and bug.txt")
        val writer = new PrintWriter(new File("bug.gv"))
        writer.write(dotifier.textBuf.mkString(""))
        writer.write("}")
        writer.close()
        val writer2 = new PrintWriter(new File("bug.txt"))
        writer2.write(textifier.text.mkString(""))
        writer2.close()
        val output = Process("dot -Tpdf -O bug.gv").lineStream
        println("Output from dot: " + output)
        System.exit(1)
    }

    // for debugging
    if (toFileDebugging)
      toFile(name, cw)
    //        debugWriteClass(getResourceAsStream(resource))

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
  def getTraceClassVisitor(next: ClassVisitor): ClassVisitor = new TraceClassVisitor(next, null)

  def getCheckClassAdapter(next: ClassVisitor): ClassVisitor = new CheckClassAdapter(next)

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

  def debugWriteClass(is: InputStream) = {
    val cr = new ClassReader(is)
    val classNode = new ClassNode(ASM5)
    cr.accept(classNode, 0)
    val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
    cr.accept(cw, 0)

    toFile(classNode.name + "_", cw)
  }
}

