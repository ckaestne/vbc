package edu.cmu.cs.vbc.utils

import java.io.PrintWriter

import org.objectweb.asm.{ClassReader, Opcodes}
import org.objectweb.asm.tree.ClassNode
import org.objectweb.asm.util.{ASMifier, TraceClassVisitor}

/**
  * @author chupanw
  */
object ClassPrinter extends App {

  val cr = new ClassReader(args(0))
  cr.accept(new TraceClassVisitor(null, new PrintWriter(System.out)), 0)

}

object ASMifierPrinter extends App {

  val cr = new ClassReader(args(0))
  cr.accept(new TraceClassVisitor(null, new ASMifier(), new PrintWriter(System.out)), 0)
}

object DotifierPrinter extends App {

  val cr = new ClassReader(args(0))
  val cn = new ClassNode(Opcodes.ASM5)
  cr.accept(cn, 0)
  cr.accept(new TraceClassVisitor(null, new Dotifier(cn), new PrintWriter(System.out)), 0)
}
