package edu.cmu.cs.vbc.utils

import java.io.PrintWriter

import org.objectweb.asm.ClassReader
import org.objectweb.asm.util.TraceClassVisitor

/**
  * @author chupanw
  */
object ClassPrinter extends App {

  val cr = new ClassReader(args(0))
  cr.accept(new TraceClassVisitor(null, new PrintWriter(System.out)), 0)

}
