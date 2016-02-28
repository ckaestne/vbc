package edu.cmu.cs.vbc

import java.lang.reflect.{Method, Modifier}

/**
  * @author chupanw
  *
  *         Simple launcher class that executes a program variationally after lifting it.
  *
  *         Provide the main class as a parameter
  */
object Launcher extends App {
    if (args.size < 1)
        throw new RuntimeException("provide main class as parameter")

    val classname = args(0)
    val liftBytecode = args.size < 2 || args(1) == "true"
    val loader: VBCClassLoader = new VBCClassLoader(liftBytecode)
    val cls: Class[_] = loader.loadClass(classname)
    invokeMain(cls)

    def invokeMain(cls: Class[_]): Unit = {
        try {
            val mtd: Method = cls.getMethod("main", classOf[Array[String]])
            val modifiers = mtd.getModifiers
            if (Modifier.isPublic(modifiers) && Modifier.isStatic(modifiers))
                mtd.invoke(null, args.tail.tail)
        } catch {
            case e: NoSuchMethodException => println("No main method found in $classname, aborting...")
        }
    }
}
