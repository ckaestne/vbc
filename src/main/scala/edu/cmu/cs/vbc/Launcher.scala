package edu.cmu.cs.vbc

import java.lang.reflect.{Method, Modifier}

import de.fosd.typechef.featureexpr.{FeatureExprFactory, FeatureExpr}
import edu.cmu.cs.varex.V

/**
  * @author chupanw
  */
object Launcher extends App {
  val isLift = args(1) == "true"
  val loader: VBCClassLoader = new VBCClassLoader(true)
  val cls: Class[_] = loader.loadClass(args(0))
  invokeMain(cls)

  def invokeMain(cls: Class[_]): Unit = {
    try {
      if (isLift) {
        val mtd: Method = cls.getMethod("main", classOf[FeatureExpr], classOf[V[Array[String]]])
        val modifiers = mtd.getModifiers;
        if (Modifier.isPublic(modifiers) && Modifier.isStatic(modifiers))
          mtd.invoke(null, FeatureExprFactory.False, V.one(args.tail.tail))
      }
      else {
        val mtd: Method = cls.getMethod("main", classOf[Array[String]])
        val modifiers = mtd.getModifiers;
        if (Modifier.isPublic(modifiers) && Modifier.isStatic(modifiers))
          mtd.invoke(null, args.tail.tail)
      }
    } catch {
      case e: NoSuchMethodException => println("No main method, aborting...")
    }
  }
}
