package edu.cmu.cs.vbc.analysis;

import org.objectweb.asm.tree.AbstractInsnNode;
import org.objectweb.asm.tree.analysis.*;

import java.util.*;

/**
 * Created by ckaestne on 1/16/2016.
 */
public class StackHeightAnalysis extends Analyzer<SourceValue> {

  public StackHeightAnalysis() {
    super(new MyInterpreter());
  }

  final Map<Integer, Set<Integer>> succ = new HashMap<>();

  private void addSucc(int a, int b) {
    if (succ.containsKey(a))
      succ.get(a).add(b);
    else {
      HashSet<Integer> v = new HashSet<>();
      v.add(b);
      succ.put(a, v);
    }
  }

  @Override
  protected boolean newControlFlowExceptionEdge(int insn, int successor) {
//    System.out.println(insn + "=>" + successor);
    addSucc(insn, successor);
    return super.newControlFlowExceptionEdge(insn, successor);
  }


  @Override
  protected void newControlFlowEdge(int insn, int successor) {
    super.newControlFlowEdge(insn, successor);
//    System.out.println(insn + "->" + successor);
    addSucc(insn, successor);
  }

  @Override
  protected Frame<SourceValue> newFrame(int nLocals, int nStack) {
    return new MyFrame(nLocals, nStack, 0, 0);
  }

  @Override
  protected Frame<SourceValue> newFrame(Frame<? extends SourceValue> src) {
    return new MyFrame(src);
  }

  public String getStackDiff(AbstractInsnNode inst) {
    Integer popped = MyFrame.elementsPoppedMap.get(inst);
    Integer pushed = MyFrame.elementsPushedMap.get(inst);
    return "[-" + popped + ",+" + pushed + "]";
  }

}

class MyFrame extends Frame<SourceValue> {

  int elementsPopped;
  int elementsPushed;

  public final static WeakHashMap<AbstractInsnNode, Integer> elementsPoppedMap = new WeakHashMap<>();
  public final static WeakHashMap<AbstractInsnNode, Integer> elementsPushedMap = new WeakHashMap<>();

  public MyFrame(int nLocals, int nStack, int elementsPopped, int elementsPushed) {
    super(nLocals, nStack);
    this.elementsPopped = elementsPopped;
    this.elementsPushed = elementsPushed;
  }

  public MyFrame(Frame<? extends SourceValue> src) {
    super(src);
    this.elementsPopped = ((MyFrame) src).elementsPopped;
    this.elementsPushed = ((MyFrame) src).elementsPushed;
    ((MyFrame) src).elementsPopped = 0;
    ((MyFrame) src).elementsPushed = 0;
  }

  @Override
  public Frame<SourceValue> init(Frame<? extends SourceValue> src) {
    this.elementsPopped = 0;
    this.elementsPushed = 0;
    return super.init(src);
  }

  @Override
  public void execute(AbstractInsnNode insn, Interpreter<SourceValue> interpreter) throws AnalyzerException {
    this.elementsPopped = 0;
    this.elementsPushed = 0;
    super.execute(insn, interpreter);
    elementsPoppedMap.put(insn, elementsPopped);
    elementsPushedMap.put(insn, elementsPushed);
  }

  @Override
  public SourceValue pop() throws IndexOutOfBoundsException {
    SourceValue v = super.pop();
    elementsPopped += v.getSize();
    return v;
  }

  @Override
  public void push(SourceValue value) throws IndexOutOfBoundsException {
    super.push(value);
    elementsPushed += value.getSize();
  }

  @Override
  public String toString() {
    return "[-" + elementsPopped + ",+" + elementsPushed + "]";
  }
}

class MyInterpreter extends SourceInterpreter {

}
