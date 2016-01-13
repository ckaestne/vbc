import java.io.{PrintWriter, ByteArrayOutputStream, InputStream}
import java.lang.reflect.Method

import org.objectweb.asm._
import org.objectweb.asm.tree._
import org.objectweb.asm.util.{TraceClassVisitor, TraceMethodVisitor}


import scala.collection.JavaConversions._

/**
  * Created by ckaestne on 12/25/2015.
  */
object ClassFilePrinter extends App {

  val name = args(0)
  val resource: String = name.replace('.', '/') + ".class"
  val is: InputStream = ClassLoader.getSystemClassLoader.getResourceAsStream(resource)
  val cr = new ClassReader(is)

  var printer = new TraceClassVisitor(new PrintWriter(System.out))


  cr.accept(printer, 0)

}