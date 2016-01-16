package edu.cmu.cs.vbc

import edu.cmu.cs.vbc.instructions._
import edu.cmu.cs.vbc.test.{InstrDBGIPrint, InstrLoadConfig}
import org.objectweb.asm.Opcodes._
import org.scalatest.FunSuite


class VBCTest extends FunSuite with DiffTestInfrastructure {

    test("simple method") {
        testMethod(new MethodNode(ACC_PUBLIC, "test", "()V", "()V", Array.empty,
            CFG(List(Block(List(InstrICONST(), InstrDBGIPrint(), InstrRETURN()))))))
    }

    test("load condition") {
        testMethod(new MethodNode(ACC_PUBLIC, "test", "()V", "()V", Array.empty,
            CFG(List(Block(List(InstrLoadConfig("A"), InstrDBGIPrint(), InstrRETURN()))))))
    }

}
