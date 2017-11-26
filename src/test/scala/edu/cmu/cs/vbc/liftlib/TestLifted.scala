//package edu.cmu.cs.vbc.liftlib
//
//import java.io.File
//import java.net.URLClassLoader
//
//import edu.cmu.cs.vbc.Lifting.liftNameDot
//import org.scalatest.FunSuite
//
//class TestLifted extends FunSuite {
//
//  val ΔlibPath = "lifted/"
//  val ΔlibURL = new File(ΔlibPath).toURI.toURL
//  val ΔlibClassloader = new ClassLoader() {
//    val urlClassloader = URLClassLoader.newInstance(Array(ΔlibURL), getClass().getClassLoader())
//
//    override def loadClass(name: String): Class[_] = {
//      println("loading " + name)
//      urlClassloader.loadClass(name)
//    }
//  }
//
//
//  test("LinkedList") {
//    val clazz = classOf[java.util.LinkedList[_]]
//    val Δclazz = ΔlibClassloader.loadClass(liftNameDot(clazz.getName))
//
//    val inst = Δclazz.newInstance().asInstanceOf[Δ.java.util.LinkedList[_]]
//
//  }
//}