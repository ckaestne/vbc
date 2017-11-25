package edu.cmu.cs.vbc

import java.io.{File, FileWriter}

import edu.cmu.cs.vbc
import edu.cmu.cs.vbc.vbytecode._
import edu.cmu.cs.vbc.vbytecode.instructions._

/**
  * compares the execution of a class (via main method, lifted using a special class loader)
  * between variational and brute-force execution
  */
trait DiffLaunchTestInfrastructure {

  /**
    * instrument byte code to log every method invocation and
    * every access to a field and the value of a return statement
    *
    * also change the way that conditional fields are initialized
    */
  def instrumentMethod(method: VBCMethodNode, clazz: VBCClassNode): VBCMethodNode = {
    instrumentCustomInit(
      // Avoid comparing java classes because we might have model classes.
      // For brute-force execution, we don't use model class (to avoid writing wrapper methods in model classes),
      // so invokevirtual and getfield will be recorded. However, for V execution, we are using model class, which
      // makes it difficult to compare the traces.
      if (clazz.name.startsWith("model/java"))
        method
      // we introduce extra method invocation instructions and return instructions while handling <clinit>.
      else if (!method.name.contains("clinit"))
        method.copy(body = CFG(method.body.blocks.map(instrumentBlock)))
      else
        method
    )
  }

  def instrumentBlock(block: Block): Block =
    Block((
      for (instr <- block.instr) yield instr match {
        case InstrINVOKEVIRTUAL(Owner("java/io/PrintStream"), MethodName("println"), MethodDesc("(Ljava/lang/String;)V"), _) =>
          // reduce output
          //          List(vbc.TraceInstr_Print(), instr)
          List(vbc.TraceInstr_Print(), InstrPOP(), InstrPOP())
        case InstrINVOKEVIRTUAL(Owner("java/io/PrintStream"), MethodName("println"), MethodDesc("(Ljava/lang/Object;)V"), _) =>
          // reduce output
          //          List(vbc.TraceInstr_Print(), instr)
          List(vbc.TraceInstr_Print(), InstrPOP(), InstrPOP())
        case InstrINVOKEVIRTUAL(Owner("java/io/PrintStream"), MethodName("println"), MethodDesc("(I)V"), _) => List(vbc.TraceInstr_PrintI(), InstrPOP(), InstrPOP())
        case InstrINVOKEVIRTUAL(owner, name, desc, _) => List(vbc.TraceInstr_S("INVK_VIRT: " + owner + ";" + name + ";" + desc), instr)
        case InstrGETFIELD(owner, name, desc) if desc.contentEquals("I") || desc.contentEquals("Z") => List(instr, vbc.TraceInstr_GetField("GETFIELD: " + owner + ";" + name + ";" + desc, desc))
        //        case InstrRETURN() => List(vbc.TraceInstr_S("RETURN"), instr)
        //        case InstrIRETURN() => List(vbc.TraceInstr_S("IRETURN"), instr)
        case instr => List(instr)
      }
      ).flatten, block.exceptionHandlers
    )

  def prepareBenchmark(method: VBCMethodNode, clazz: VBCClassNode): VBCMethodNode = instrumentCustomInit(avoidOutput(method, clazz))

  def avoidOutput(method: VBCMethodNode, clazz: VBCClassNode): VBCMethodNode = method.copy(body = CFG(method.body.blocks.map(avoidOutput)))

  def avoidOutput(block: Block): Block =
    Block((
      for (instr <- block.instr) yield instr match {
        case InstrINVOKEVIRTUAL(Owner("java/io/PrintStream"), MethodName("println"), MethodDesc("(Ljava/lang/String;)V"), _) => List(InstrPOP(), InstrPOP())
        case InstrINVOKEVIRTUAL(Owner("java/io/PrintStream"), MethodName("println"), MethodDesc("(Ljava/lang/Object;)V"), _) => List(InstrPOP(), InstrPOP())
        case InstrINVOKEVIRTUAL(Owner("java/io/PrintStream"), MethodName("println"), MethodDesc("(I)V"), _) => List(InstrPOP(), InstrPOP())
        case instr => List(instr)
      }
      ).flatten, block.exceptionHandlers
    )

  def instrumentCustomInit(method: VBCMethodNode): VBCMethodNode = method.copy(body = CFG(method.body.blocks.map(instrumentCustomInit)))

  def instrumentCustomInit(block: Block): Block =
    Block(
      (for (instr <- block.instr) yield instr match {
        //replace initialization of conditional fields
        case InstrINIT_FIELDS(isStatic, _) => vbc.TraceInstr_ConfigInit(isStatic)
        case instr => instr
      }), block.exceptionHandlers
    )


  //  def checkCrash(clazz: Class[_]): Unit = testMain(clazz, false)

  /**
    * tests automatic lifting of a class file:
    *
    * @param clazz                         class to execute, with main method
    * @param executeLifted                 execute the lifted code, test for crashing bugs
    * @param executeLiftedSilent           execute the lifted code, but try to surpress all output to the console in the original program
    * @param compareTraceAgainstBruteForce execute lifted code once and compare against every single plain execution.
    *                                      compares method call sequence and a few select instructions, but not values
    * @param runBenchmark                  compare performance of lifted vs unlifted programs; repeated executions and executes all unlifted configurations
    * @param fm
    * @param configFile
    */
  def testMain(clazz: Class[_],
               executeLifted: Boolean = true,
               executeLiftedSilent: Boolean = true,
               compareTraceAgainstBruteForce: Boolean = true,
               runBenchmark: Boolean = true,
               fm: (Map[String, Boolean]) => Boolean = _ => true, configFile: Option[String] = None): Unit = {

    val classname = clazz.getName
    val origClassLoader = this.getClass.getClassLoader

    if (executeLifted || executeLiftedSilent) {
      //test uninstrumented variational execution to see whether it crashes
      //VBCLauncher.launch(classname)
      val rewrite: (VBCMethodNode, VBCClassNode) => VBCMethodNode = if (executeLiftedSilent) avoidOutput else (a, b) => a
      val testCrashLoader: VBCClassLoader = new VBCClassLoader(origClassLoader, true, rewrite, configFile = configFile)
      Thread.currentThread().setContextClassLoader(testCrashLoader)
      val testCrash = testCrashLoader.loadClass(classname)
      VBCLauncher.invokeMain(testCrash, new Array[String](0))
    }

    //test instrumented version, executed variationally
    if (compareTraceAgainstBruteForce || runBenchmark) {
      TestTraceOutput.trace = Nil
      TraceConfig.options = Set()
      //variational execution
      val vloader: VBCClassLoader = new VBCClassLoader(origClassLoader, true, rewriter = instrumentMethod, configFile = configFile)
      Thread.currentThread().setContextClassLoader(vloader)
      val vcls: Class[_] = vloader.loadClass(classname)
      VBCLauncher.invokeMain(vcls, new Array[String](0))

      val vtrace = TestTraceOutput.trace
      val usedOptions = TraceConfig.options.map(_.feature)

      println("Used Options: " + TraceConfig.options.mkString(", "))

      //instrumented brute-force execution
      if (compareTraceAgainstBruteForce) {
        val loader: VBCClassLoader = new VBCClassLoader(origClassLoader, false, instrumentMethod, toFileDebugging = true, configFile = configFile)
        Thread.currentThread().setContextClassLoader(loader)
        val cls: Class[_] = loader.loadClass(classname)
        //run against brute force instrumented execution and compare traces
        for ((sel, desel) <- explode(usedOptions.toList) if fm(configToMap((sel, desel)))) {
          println("executing config [" + sel.mkString(", ") + "]")
          TestTraceOutput.trace = Nil
          TraceConfig.config = configToMap((sel, desel))
          VBCLauncher.invokeMain(cls, new Array[String](0))
          val atrace = TestTraceOutput.trace

          //get the trace from the v execution relevant for this config and compare
          val filteredvtrace = vtrace.filter(_._1.evaluate(sel.toSet))
          compareTraces(sel, atrace.map(_._2).reverse, filteredvtrace.map(_._2).reverse)
        }
      }

      if (runBenchmark) {
        //run benchmark (without instrumentation)
        val vbenchmarkloader: VBCClassLoader = new VBCClassLoader(origClassLoader, true, prepareBenchmark, configFile = configFile)
        val benchmarkloader: VBCClassLoader = new VBCClassLoader(origClassLoader, false, prepareBenchmark, configFile = configFile)
        benchmark(classname, vbenchmarkloader, benchmarkloader, usedOptions, fm)
      }
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

  /**
    * Randomly assign true or false to each option
    *
    * @param fs  List of options in the program.
    * @param num Number of selections.
    * @return List of pairs, each pair consists of a list selected options and a list of deselected options. Might
    *         contain duplicate pairs because of the randomness.
    */
  def selectOptRandomly(fs: List[Feature], num: Int): List[Config] = {
    import scala.util.Random
    require(num > 0)
    if (fs.isEmpty) List((Nil, Nil))
    else {
      (0 to num).toList.map(_ => Random.shuffle(fs).splitAt(Random.nextInt(fs.size)))
    }
  }


  def configToMap(c: Config): Map[String, Boolean] = {
    var r = Map[String, Boolean]()
    for (sel <- c._1)
      r += (sel -> true)
    for (desel <- c._2)
      r += (desel -> false)
    r
  }

  def compareTraces(ctx: List[Feature], expected: List[String], actual: List[String]): Unit = {
    if (expected != actual) {
      val expectedOut = new FileWriter(new File("expected"))
      val foundOut = new FileWriter(new File("found"))
      expectedOut.write("EXPECTED (plain execution in [" + ctx.mkString(", ") + "]): \n")
      expectedOut.write(expected.mkString("\n"))
      foundOut.write("FOUND (variational execution in [" + ctx.mkString(", ") + "]):\n")
      foundOut.write(actual.mkString("\n"))
      expectedOut.close()
      foundOut.close()
      throw new RuntimeException("mismatch between plain execution and variational execution in config [" + ctx.mkString(", ") + "], check the log files")
    }
  }


  def benchmark(classname: String, vloader: VBCClassLoader, loader: VBCClassLoader, configOptions: Set[String], fm: (Map[String, Boolean]) => Boolean): Unit = {
    import org.scalameter._

    //measure V execution
    Thread.currentThread().setContextClassLoader(vloader)
    val testVClass: Class[_] = vloader.loadClass(classname)
    val vtime = config(
      Key.exec.benchRuns -> 20
    ) withWarmer {
      new Warmer.Default
    } setUp { _ =>
      TestTraceOutput.trace = Nil
      TraceConfig.config = Map()
    } measure {
      //      Profiler.reset()
      VBCLauncher.invokeMain(testVClass, new Array[String](0))
      //      Profiler.report()
    }
    //        println(s"Total time V: $time")


    Thread.currentThread().setContextClassLoader(loader)
    lazy val testClass: Class[_] = loader.loadClass(classname)
    //measure brute-force execution
    val configs =
      if (configOptions.size < 20)
        explode(configOptions.toList)
      else
        selectOptRandomly(configOptions.toList, 1000000) // close to 2^20
    val bftimes = for ((sel, desel) <- configs if fm(configToMap((sel, desel))))
      yield config(
        Key.exec.benchRuns -> 20
      ) withWarmer {
        new Warmer.Default
      } setUp { _ =>
        TestTraceOutput.trace = Nil
        TraceConfig.config = configToMap((sel, desel))
      } measure {
        VBCLauncher.invokeMain(testClass, new Array[String](0))
      }


    val avgTime = bftimes.map(_.value).sum / bftimes.size
    println(s"VExecution time [$classname]: " + vtime)
    println(s"Execution time [$classname]: " + avgTime + bftimes.mkString(" (", ",", ")"))
    println(s"Slowdown [$classname]: " + vtime.value / avgTime)

  }

}
