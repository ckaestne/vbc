package edu.cmu.cs.vbc

import edu.cmu.cs.vbc.test.{TestTraceOutput, TraceInstr_GetField, TraceInstr_Print, TraceInstr_S}
import edu.cmu.cs.vbc.vbytecode.instructions.{InstrGETFIELD, InstrINVOKEVIRTUAL, InstrIRETURN, InstrRETURN}
import edu.cmu.cs.vbc.vbytecode.{Block, CFG, VBCMethodNode}
import org.scalatest.FunSuite


/**
  * simple starter, checks for successful execution without crashes, no assertion checking
  */
class TestLaunch extends FunSuite with DiffTestInfrastructure {

    /**
      * instrument byte code to log every method invocation and
      * every access to a field and the value of a return statement
      */
    def instrumentMethod(method: VBCMethodNode): VBCMethodNode = method.copy(body = CFG(method.body.blocks.map(instrumentBlock)))

    def instrumentBlock(block: Block): Block = {
        val v = Block((
            for (instr <- block.instr) yield instr match {
                case InstrINVOKEVIRTUAL("java/io/PrintStream", "println", "(Ljava/lang/String;)V", _) => List(TraceInstr_Print(), instr)
                case InstrINVOKEVIRTUAL("java/io/PrintStream", "println", "(Ljava/lang/Object;)V", _) => List(TraceInstr_Print(), instr)
                case InstrINVOKEVIRTUAL(owner, name, desc, _) => List(TraceInstr_S(owner + ";" + name + ";" + desc), instr)
                case InstrGETFIELD(owner, name, desc) if (desc == "I" || desc == "Z") => List(instr, TraceInstr_GetField(owner + ";" + name + ";" + desc, desc))
                case InstrRETURN() => List(TraceInstr_S("RETURN"), instr)
                case InstrIRETURN() => List(TraceInstr_S("IRETURN"), instr)
                case instr => List(instr)
            }
            ).flatten: _*
        )
        v
    }


    def testMain(classname: String): Unit = {
        //test plain variational execution to see whether it crashes
        VBCLauncher.launch(classname)

        //test instrumented version, executed variationally
        TestTraceOutput.trace = Nil
        val loader: VBCClassLoader = new VBCClassLoader(this.getClass.getClassLoader, true, instrumentMethod)
        val cls: Class[_] = loader.loadClass(classname)
        VBCLauncher.invokeMain(cls, new Array[String](0))
        val vtrace = TestTraceOutput.trace

        for ((f, s) <- vtrace)
            println(s"<$f>$s")

        //TODO run against brute force execution and compare traces

        //TODO run benchmark
    }

    test("ifelse") {
        testMain("edu.cmu.cs.vbc.prog.IfElseExample")
    }
    ignore("bankaccount") {
        testMain("edu.cmu.cs.vbc.prog.bankaccount.Main")
    }

}
