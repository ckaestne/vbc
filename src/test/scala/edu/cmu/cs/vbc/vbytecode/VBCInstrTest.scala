package edu.cmu.cs.vbc.vbytecode

import edu.cmu.cs.vbc
import edu.cmu.cs.vbc.vbytecode.instructions._
import edu.cmu.cs.vbc.{DiffMethodTestInfrastructure, InstrLoadConfig}
import org.scalatest.FunSuite


class VBCInstrTest extends FunSuite with DiffMethodTestInfrastructure {

  test("simple method") {
    simpleMethod(InstrICONST(0), vbc.InstrDBGIPrint(), InstrRETURN())
  }

  test("load condition") {
    simpleMethod(InstrLoadConfig("A"), vbc.InstrDBGIPrint(), InstrRETURN())
  }

  test("conditional IADD") {
    simpleMethod(InstrICONST(4), InstrLoadConfig("A"), InstrIADD(), vbc.InstrDBGIPrint(), InstrRETURN())
  }


  test("2 conditional IADD") {
    simpleMethod(InstrLoadConfig("B"), InstrLoadConfig("A"), InstrIADD(), vbc.InstrDBGIPrint(), InstrRETURN())
  }

  test("LOAD, STORE") {
    val l = new LocalVar("v", "I")
    simpleMethod(InstrLoadConfig("A"), InstrISTORE(l), InstrILOAD(l), vbc.InstrDBGIPrint(), InstrRETURN())
  }

  test("LOAD, STORE, IINC") {
    val l = new LocalVar("v", "I")
    simpleMethod(InstrLoadConfig("A"), InstrISTORE(l), InstrIINC(l, 1), InstrILOAD(l), vbc.InstrDBGIPrint(), InstrRETURN())
  }


  test("INEG") {
    simpleMethod(InstrICONST(1), InstrINEG(), vbc.InstrDBGIPrint(), InstrRETURN())
  }

}
