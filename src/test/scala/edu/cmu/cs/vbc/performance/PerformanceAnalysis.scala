package edu.cmu.cs.vbc.performance

import java.io.{BufferedWriter, FileWriter}

import de.fosd.typechef.featureexpr.FeatureExprFactory
import edu.cmu.cs.vbc._
import edu.cmu.cs.vbc.analysis.VBCFrame
import edu.cmu.cs.vbc.vbytecode._
import edu.cmu.cs.vbc.vbytecode.instructions._
import org.objectweb.asm.{MethodVisitor, Opcodes}
import org.scalatest.FunSuite

/**
  * Performance instrumentation and analysis framework
  */
object PerformanceAnalysisMain extends PerformanceAnalysis with App {
  FeatureExprFactory.setDefault(FeatureExprFactory.bdd)

  def config(c: String) = Set("SEARCH", "BASE", "WEIGHTED", "CYCLE", "DFS", "STRONGLYCONNECTED", "DIRECTED", "TRANSPOSE", "SHORTEST", "NUMBER", "Src", "SrcProg", "Src2") contains c

  debugPerformanceOverhead(classOf[edu.cmu.cs.vbc.prog.gpl.Main],
    config, // allyesconfig
    configFile = Some("gpl.conf"))


}

trait PerformanceAnalysis {

  type Rewriter = (VBCMethodNode, VBCClassNode) => VBCMethodNode
  type Configuration = (String) => Boolean

  def combine(second: Rewriter, first: Rewriter): Rewriter = (method, cls) => second(first(method, cls), cls)


  def mergeTraces(plainTraces: List[PerformanceTrace]): List[(Int, String, List[Long])] = {
    val tc = plainTraces.head.tc
    val length = plainTraces.head.trace.length

    var traces = plainTraces.map(_.trace)

    traces.map(_.length).foreach(l => assert(l == length, "traces with different length"))

    var result: List[(Int, String, List[Long])] = Nil
    while (traces.head.nonEmpty) {
      //      if (traces.head.head._1 < 3)
      result ::= (traces.head.head._1, tc.methods(traces.head.head._2), traces.map(_.head._3))
      traces = traces.map(_.tail)
    }

    result
  }

  //  def avg(l: List[Long]): Long = l.foldRight(0l)(_ + _) / l.length / 1000000

  class Statistics(data: List[Double]) {
    var size = data.length

    def getMean: Double = {
      var sum = 0.0
      for (a <- data) {
        sum += a
      }
      sum / size
    }

    def getVariance: Double = {
      val mean = getMean
      var temp = 0d
      for (a <- data) {
        temp += (a - mean) * (a - mean)
      }
      temp / (size - 1)
    }

    def getStdDev: Double = Math.sqrt(getVariance)

    //    def median: Double = {
    //      Arrays.sort(data)
    //      if (data.length % 2 == 0) return (data((data.length / 2) - 1) + data(data.length / 2)) / 2.0
    //      data(data.length / 2)
    //    }
  }

  def produceReport(plainTraces: List[PerformanceTrace], liftedTraces: List[PerformanceTrace]): Unit = {

    var plainTrace = mergeTraces(plainTraces)
    var liftedTrace = mergeTraces(liftedTraces)


    val writer = new BufferedWriter(new FileWriter("perfreport.html"))
    writer.write(
      """
        <style TYPE="text/css">
        <!--
        table {width: 100%;}
        tr:nth-child(even) {background-color: #f2f2f2;}
        td {
     max-width: 100px;
     overflow: hidden;
     text-overflow: ellipsis;
     white-space: nowrap;
 }
        --></style>
      """)
    writer.write("<div style=\"overflow-x:auto;\"><table><tr><th>Plain</th><th>Time</th><th>Lifted</th><th>Time</th><th>Slowdown</th></tr>")
    while (plainTrace.nonEmpty || liftedTrace.nonEmpty) {
      val plainStats = plainTrace.headOption.map(h => new Statistics(h._3.map(_.toDouble)))
      val liftedStats = liftedTrace.headOption.map(h => new Statistics(h._3.map(_.toDouble)))

      val max = Math.max(plainStats.map(_.getMean).getOrElse(0d), liftedStats.map(_.getMean).getOrElse(0d))

      var row = ("<tr><td>")


      if (plainTrace.nonEmpty) {
        row += ("&nbsp;" * 4 * plainTrace.head._1 +
          plainTrace.head._2 +
          "</td><td>%1.0f&#177;%1.0f".format(plainStats.get.getMean / 1000000, plainStats.get.getStdDev / 1000000))
        plainTrace = plainTrace.tail
      } else row += ("</td><td>")

      row += ("</td><td>")

      if (liftedTrace.nonEmpty) {
        val stats = new Statistics(liftedTrace.head._3.map(_.toDouble))
        row += ("&nbsp;&nbsp;" * liftedTrace.head._1 +
          liftedTrace.head._2 +
          "</td><td>%1.0f&#177;%1.0f".format(liftedStats.get.getMean / 1000000, liftedStats.get.getStdDev / 1000000))
        liftedTrace = liftedTrace.tail
      } else row += ("</td><td>")

      val slowDown = plainStats.flatMap(p => liftedStats.map(l => l.getMean / p.getMean))

      row += ("</td><td>%1.1f" format slowDown.getOrElse(0))

      row += ("</td></tr>")

      if (max >= 1000000d)
        writer.write(row)


    }


    writer.write("</table></div>")

    writer.close()

  }

  def debugPerformanceOverhead(clazz: Class[_],
                               config: Configuration,
                               configFile: Option[String] = None): Unit = {

    val classname = clazz.getName
    val origClassLoader = this.getClass.getClassLoader

    val performanceCollector = new PerformanceCollector()

    val liftingClassLoader: VBCClassLoader = new VBCClassLoader(origClassLoader, true,
      performanceCollector.rewrite,
      config = (a) => Some(config(a)),
      configFile = configFile)
    val plainClassLoader: VBCClassLoader = new VBCClassLoader(origClassLoader, false,
      performanceCollector.rewrite,
      config = (a) => Some(config(a)),
      configFile = configFile)

    //make sure all classes are loaded by executing the full program
    Thread.currentThread().setContextClassLoader(liftingClassLoader)
    val liftedClass = liftingClassLoader.loadClass(classname)
    VBCLauncher.invokeMain(liftedClass, new Array[String](0))

    Thread.currentThread().setContextClassLoader(plainClassLoader)
    val plainClass = plainClassLoader.loadClass(classname)
    VBCLauncher.invokeMain(plainClass, new Array[String](0))


    //measure time 5 times both lifted and unlifted:
    var liftedTraces: List[PerformanceTrace] = Nil
    var plainTraces: List[PerformanceTrace] = Nil
    for (i <- 1 to 5) {
      val liftedTrace = new PerformanceTrace(performanceCollector)
      TimeUtil.setPerformanceTrace(liftedTrace)
      VBCLauncher.invokeMain(liftedClass, new Array[String](0))
      liftedTraces ::= liftedTrace

      val plainTrace = new PerformanceTrace(performanceCollector)
      TimeUtil.setPerformanceTrace(plainTrace)
      VBCLauncher.invokeMain(plainClass, new Array[String](0))
      plainTraces ::= plainTrace

      //      println("summary: ")
      //      plainTrace.trace.take(5).foreach(t=>println("  "*t._1+plainTrace.tc.methods(t._2)+": "+(t._3/1000000)))
      //      plainTrace.trace.takeRight(5).foreach(t=>println("  "*t._1+plainTrace.tc.methods(t._2)+": "+(t._3/1000000)))
    }

    liftedTraces.map(t => println("lifted " + (t.trace.head._3 / 1000000) + ", " + t.trace.length))
    plainTraces.map(t => println("plain " + (t.trace.head._3 / 1000000) + ", " + t.trace.length))

    produceReport(plainTraces, liftedTraces)
  }
}

class PerformanceCollector {
  type Rewriter = (VBCMethodNode, VBCClassNode) => VBCMethodNode


  class MethodTimer(methodId: Int) {

    import Opcodes._

    var timeVariable: Option[LocalVar] = None

    class StartTimer extends Instruction {
      override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
        timeVariable = Some(env.freshLocalVar("$time", "J", LocalVar.noInit))
        mv.visitLdcInsn(Integer.valueOf(methodId))
        mv.visitMethodInsn(INVOKESTATIC, "edu/cmu/cs/vbc/performance/TimeUtil", "methodEnter", "(I)V", false)
        mv.visitMethodInsn(INVOKESTATIC, "java/lang/System", "nanoTime", "()J", false)
        mv.visitVarInsn(LSTORE, env.getVarIdx(timeVariable.get))
      }

      override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = toByteCode(mv, env, block)

      override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Set[Instruction]) = (s, Set())
    }

    class EndTimer extends Instruction {
      override def toByteCode(mv: MethodVisitor, env: MethodEnv, block: Block): Unit = {
        mv.visitMethodInsn(INVOKESTATIC, "java/lang/System", "nanoTime", "()J", false)
        mv.visitVarInsn(LLOAD, env.getVarIdx(timeVariable.get))
        mv.visitInsn(LSUB)
        mv.visitLdcInsn(Integer.valueOf(methodId))
        mv.visitMethodInsn(INVOKESTATIC, "edu/cmu/cs/vbc/performance/TimeUtil", "methodTime", "(JI)V", false)
      }

      override def toVByteCode(mv: MethodVisitor, env: VMethodEnv, block: Block): Unit = toByteCode(mv, env, block)

      override def updateStack(s: VBCFrame, env: VMethodEnv): (VBCFrame, Set[Instruction]) = (s, Set())
    }


  }

  var methodIds: Map[String, Int] = Map()
  var methods: Map[Int, String] = Map()

  def rewrite: Rewriter = (method, cls) =>
    if (cls.name startsWith "model/") method else {
      val name = cls.name + "." + method.name + method.desc
      val methodId =
        if (methodIds contains name) methodIds(name)
        else {
          val newId = methodIds.size + 1
          methodIds += (name -> newId)
          methods += (newId -> name)
          newId
        }
      val timer = new MethodTimer(methodId)

      def insertStart(blockList: List[Block]) =
        if (blockList.isEmpty) blockList else
          blockList.head.copy(instr = new timer.StartTimer() +: blockList.head.instr) :: blockList.tail

      def instrumentEnd(block: Block): Block =
        block.copy(instr = block.instr.flatMap({
          case r: ReturnInstruction => List(new timer.EndTimer(), r)
          case e => List(e)
        }))

      method.copy(body = CFG(insertStart(method.body.blocks).map(instrumentEnd)))
    }

}