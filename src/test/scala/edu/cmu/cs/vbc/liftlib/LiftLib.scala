package edu.cmu.cs.vbc.liftlib

import java.io.File
import java.net.{URL, URLClassLoader}
import java.util
import java.util.jar.JarEntry
import java.util.jar.JarFile

import edu.cmu.cs.vbc.{Lifting, TestNonVLiftPolicy, VBCClassLoader}

import scala.collection.JavaConversions._
import Lifting._
import edu.cmu.cs.vbc.liftlib.LiftLib.{getClass, libURL}
import org.scalatest.FunSuite

object LiftLib extends App {

  val libPath = "/usr/lib/jvm/java-8-oracle/jre/lib/rt.jar"
  val libURL = new File(libPath).toURI.toURL

  //  val l = new ClassLoader() {
  //    override def loadClass(n: String) = {
  //      println(s"++ $n")
  //      super.loadClass(n)
  //    }
  //  }


  val libClassloader = URLClassLoader.newInstance(Array(libURL), getClass().getClassLoader())

  val liftingLibClassloader: VBCClassLoader = new VBCClassLoader(libClassloader, false, toFileDebugging = true, liftingPolicy = TestNonVLiftPolicy)
  Thread.currentThread().setContextClassLoader(liftingLibClassloader)

//    liftingLibClassloader.loadClass("edu.cmu.cs.vbc.A")
//    liftingLibClassloader.loadClass("Δ.edu.cmu.cs.vbc.C")
//    liftingLibClassloader.loadClass("Δ.java.util.AbstractList")


  val jarFile = new JarFile(libPath)
  for (entry <- jarFile.entries().toSeq) {
    val name = entry.getName
    if ((name startsWith "java/") && (name endsWith ".class") && !(name contains "package-info") && !(name contains "java/applet"))
      if (!(name contains "FileSystem") && !(name == "java/lang/Object.class") && !(name == "java/lang/String.class")) {
        println(name)
        liftingLibClassloader.loadClass(liftNameDot(name.dropRight(6).replace("/", ".")))
        //        Class.forName(name.dropRight(6).replace("/", "."), true, liftingLibClassloader)
      }
  }


}




