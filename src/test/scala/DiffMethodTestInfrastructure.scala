package edu.cmu.cs.vbc

import java.io.PrintWriter
import java.lang.reflect.InvocationTargetException

import de.fosd.typechef.conditional.{ConditionalLib, Opt}
import de.fosd.typechef.featureexpr.{FeatureExpr, FeatureExprFactory}
import edu.cmu.cs.varex.{V, VException, VHelper}
import edu.cmu.cs.vbc.test.TestOutput.TOpt
import edu.cmu.cs.vbc.test.{Config, InstrLoadConfig, TestOutput}
import edu.cmu.cs.vbc.vbytecode.instructions._
import edu.cmu.cs.vbc.vbytecode.{Block, _}
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.util.TraceClassVisitor
import org.objectweb.asm.{ClassReader, ClassWriter}

/**
  * compares the execution of two methods
  */
trait DiffMethodTestInfrastructure {

  class MyClassLoader extends ClassLoader(this.getClass.getClassLoader) {
    def defineClass(name: String, b: Array[Byte]): Class[_] =
      defineClass(name, b, 0, b.length)

  }


  case class TestClass(m: VBCMethodNode, extraMethods: Seq[VBCMethodNode] = Nil) {
    private def createClass(testmethod: VBCMethodNode): VBCClassNode = {
      val constr = new VBCMethodNode(ACC_PUBLIC, "<init>", "()V", Some("()V"), Nil,
        CFG(List(
          Block(List(InstrALOAD(new Parameter(0, "this")),
            InstrINVOKESPECIAL("java/lang/Object", "<init>", "()V", false)), Nil),
          Block(List(InstrRETURNVoid()), Nil)
        )))
      new VBCClassNode(V1_8, ACC_PUBLIC, "Test", None, "java/lang/Object", Nil, Nil,
        List(constr, testmethod) ++ extraMethods)

    }

    def toByteCode(cw: ClassWriter) = {
      createClass(m).toByteCode(cw)
    }

    def toVByteCode(cw: ClassWriter) = {
      createClass(m).toVByteCode(cw)
    }

  }


  private def splitBlockOnMethod(block: Block): List[Block] = {
    var results = List[Block]()
    var instrs = List[Instruction]()
    for (i <- block.instr) {
      instrs = instrs :+ i
      if (i.isJumpInstr) {
        results = results :+ Block(instrs, block.exceptionHandlers)
        instrs = List()
      }
    }
    if (instrs.nonEmpty)
      results = results :+ Block(instrs, block.exceptionHandlers)
    results
  }

  protected def splitBlocksOnMethods(blocks: List[Block]): List[Block] = {
    var blockIdxMap = Map[Int, Int]()
    var updatedBlockIdx = 0
    val newBlocks = for (blockIdx <- blocks.indices) yield {
      val block = blocks(blockIdx)
      val replacement = if (block.instr.dropRight(1).exists(_.isJumpInstr))
        splitBlockOnMethod(block)
      else List(block)
      blockIdxMap += (blockIdx -> updatedBlockIdx)
      updatedBlockIdx += replacement.size
      replacement
    }

    newBlocks.flatten.map(updateBlockIdx(blockIdxMap, _)).toList
  }

  private def updateBlockIdx(updatedBlockIdx: Map[Int, Int], block: Block): Block =
    block.copy(instr = block.instr.map({
      case InstrIFEQ(idx) => InstrIFEQ(updatedBlockIdx(idx))
      case InstrIFNE(idx) => InstrIFNE(updatedBlockIdx(idx))
      case InstrIFGE(idx) => InstrIFGE(updatedBlockIdx(idx))
      case InstrIFGT(idx) => InstrIFGT(updatedBlockIdx(idx))
      case InstrIF_ICMPEQ(idx) => InstrIF_ICMPEQ(updatedBlockIdx(idx))
      case InstrIF_ICMPGE(idx) => InstrIF_ICMPGE(updatedBlockIdx(idx))
      case InstrIF_ICMPLT(idx) => InstrIF_ICMPLT(updatedBlockIdx(idx))
      case InstrIF_ICMPNE(idx) => InstrIF_ICMPNE(updatedBlockIdx(idx))
      case InstrGOTO(idx) => InstrGOTO(updatedBlockIdx(idx))
      case x: InstrUnaryIF => assert(false, "unsupported if instruction"); x
      case x: InstrBinaryIF => assert(false, "unsupported if instruction"); x
      case x => x
    }))


  def loadTestClass(clazz: TestClass): Class[_] = {
    val cw = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS)
    clazz.toByteCode(cw)
    val byte = cw.toByteArray
    val printer = new TraceClassVisitor(new PrintWriter(System.out))
    new ClassReader(byte).accept(printer, 0)
    val myClassLoader = new MyClassLoader
    myClassLoader.defineClass("Test", byte)
  }

  def loadVTestClass(clazz: TestClass): Class[_] = {
    val vcw = new ClassWriter(ClassWriter.COMPUTE_FRAMES | ClassWriter.COMPUTE_MAXS)
    clazz.toVByteCode(vcw)
    val vbyte = vcw.toByteArray

    val printer = new TraceClassVisitor(new PrintWriter(System.out))
    new ClassReader(vbyte).accept(printer, 0)
    val myVClassLoader = new MyClassLoader
    myVClassLoader.defineClass("Test", vbyte)
  }


  /**
    * test method `m`, additional methods may be provided if method invocations
    * are to be tested.
    */
  def testMethod(m: VBCMethodNode, extraMethods: VBCMethodNode*): Unit = {

    val clazz = new TestClass(m, extraMethods)


    val configOptions: Set[String] = getConfigOptions(m)


    //        val resource: String = "edu.cmu.cs.vbc.Tmp".replace('.', '/') + ".class"
    //        val is: InputStream = myClassLoader.getResourceAsStream(resource)
    //        new ClassReader(is).accept(printer, 0)


    //        val testClass = myClassLoader.defineClass("Test", byte)

    val testClass = loadTestClass(clazz)
    val testVClass = loadVTestClass(clazz)

    val vresult: List[TOpt[String]] = executeV(testVClass, m.name)
    val bruteForceResult = executeBruteForce(testClass, m.name, configOptions)
    println("expected " + bruteForceResult.reverse)
    println("found    " + vresult.reverse)

    compare(bruteForceResult.reverse, vresult.reverse)

    benchmark(testVClass, testClass, m.name, configOptions)
  }

  def getConfigOptions(m: VBCMethodNode): Set[String] = {
    val configOptions: Set[String] =
      (for (block <- m.body.blocks; instr <- block.instr; if instr.isInstanceOf[InstrLoadConfig]) yield instr.asInstanceOf[InstrLoadConfig].config).toSet
    configOptions
  }

  private def compare(expected: List[TOpt[String]], result: List[TOpt[String]]): Unit = {
    def compareOne(ctx: FeatureExpr, e: String, f: String): Unit =
      assert(ctx.isContradiction || e.trim == f.trim, s"mismatch between expected output and actual output in context $ctx: \nEXPECTED:\n$e\nFOUND:\n$f\nALL:\n" + render(result))

    val allExpected = ConditionalLib.explodeOptList(expected).map(_.mkString)
    val allFound = ConditionalLib.explodeOptList(result).map(_.mkString)
    ConditionalLib.vmapCombination(allExpected, allFound, FeatureExprFactory.True, compareOne)
  }

  private def render(result: List[TOpt[String]]): String =
    result.map(o => "[#condition " + o.feature + "]" + o.entry).mkString

  def executeBruteForce(testClass: Class[_], method: String, configOptions: Set[String]) = {
    //now all the separate executions
    var bruteForceResult: List[Opt[String]] = Nil
    val configs = explode(configOptions.toList)
    for ((sel, desel) <- configs) {
      TestOutput.output = Nil
      Config.configValues = (sel.map(_ -> 1) ++ desel.map(_ -> 0)).toMap
      val config = (sel.map(FeatureExprFactory.createDefinedExternal)
        ++ desel.map(FeatureExprFactory.createDefinedExternal(_).not)).
        fold(FeatureExprFactory.True)(_ and _)

      val testObject = testClass.newInstance()
      executePlain(testObject, testClass, method)

      bruteForceResult = TestOutput.output.map(_.and(config)) ++ bruteForceResult
    }
    bruteForceResult
  }

  def executePlain(obj: Any, clazz: Class[_], method: String): Any = {
    try {
      clazz.getMethod(method).invoke(obj)
    } catch {
      case e: InvocationTargetException =>
        TestOutput.printS("terminated:" + e.getTargetException.getClass.getCanonicalName + ":" + e.getTargetException.getMessage)
    }
  }

  def executeV(testVClass: Class[_], method: String): List[TOpt[String]] = {
    //VExecution first
    TestOutput.output = Nil
    Config.configValues = Map()
    val ctx = FeatureExprFactory.True

    val constructor = testVClass.getConstructor(classOf[FeatureExpr])
    val testVObject = constructor.newInstance(ctx)
    executeV(testVObject, testVClass, method, ctx)
    val vresult = TestOutput.output
    vresult
  }

  def executeV(obj: Any, clazz: Class[_], method: String, ctx: FeatureExpr): Any = {
    import scala.collection.JavaConversions._
    try {
      clazz.getMethod(method, classOf[FeatureExpr]).invoke(obj, ctx)
    } catch {
      case e: InvocationTargetException =>
        if (e.getTargetException.isInstanceOf[VException]) {
          val ve = e.getTargetException.asInstanceOf[VException]
          for ((c, e) <- VHelper.explode(ctx and ve.getExceptionCondition, ve.getExceptions))
            TestOutput.printVS(V.one("terminated:" + e.getClass.getCanonicalName + ":" + e.getMessage), c)
        } else
          TestOutput.printVS(V.one("terminated:" + e.getTargetException.getClass.getCanonicalName + ":" + e.getTargetException.getMessage), ctx)
    }
  }

  type Feature = String
  type Config = (List[Feature], List[Feature])

  def explode(fs: List[Feature]): List[Config] = {
    if (fs.isEmpty) List((Nil, Nil))
    else if (fs.size == 1) List((List(fs.head), Nil), (Nil, List(fs.head)))
    else {
      val r = explode(fs.tail)
      r.map(x => (fs.head :: x._1, x._2)) ++ r.map(x => (x._1, fs.head :: x._2))
    }
  }


  def benchmark(testVClass: Class[_], testClass: Class[_], method: String, configOptions: Set[String]): Unit = {
    import org.scalameter._

    //measure V execution
    var testObject: Any = null
    var _ctx: FeatureExpr = null
    val vtime = config(
      Key.exec.benchRuns -> 20
    ) withWarmer {
      new Warmer.Default
      //        } withMeasurer {
      //            new Measurer.IgnoringGC
    } setUp { _ =>
      TestOutput.output = Nil
      Config.configValues = Map()
      _ctx = FeatureExprFactory.True
      val constructor = testVClass.getConstructor(classOf[FeatureExpr])
      testObject = constructor.newInstance(_ctx)
    } measure {
      executeV(testObject, testVClass, method, _ctx)
    }
    //        println(s"Total time V: $time")


    //measure brute-force execution
    val configs = explode(configOptions.toList)
    val bftimes = for ((sel, desel) <- configs)
      yield config(
        Key.exec.benchRuns -> 20
      ) withWarmer {
        new Warmer.Default
        //        } withMeasurer {
        //            new Measurer.IgnoringGC
      } setUp { _ =>
        TestOutput.output = Nil
        Config.configValues = (sel.map(_ -> 1) ++ desel.map(_ -> 0)).toMap
        testObject = testClass.newInstance()
      } measure {
        executePlain(testObject, testClass, method)
      }


    val avgTime = bftimes.map(_.value).sum / bftimes.size
    println("VExecution time: " + vtime)
    println("Execution time: " + avgTime + bftimes.mkString(" (", ",", ")"))
    println("Slowdown: " + vtime.value / avgTime)

  }


}
