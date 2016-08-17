package edu.cmu.cs.vbc.loader

import java.io.InputStream

import edu.cmu.cs.vbc.vbytecode._
import edu.cmu.cs.vbc.vbytecode.instructions._
import org.objectweb.asm.Opcodes._
import org.objectweb.asm.tree._
import org.objectweb.asm.{ClassReader, Opcodes, Type}

import scala.collection.JavaConversions._

/**
  * My class adapter using the tree API
  */
class Loader {

  def loadClass(is: InputStream): VBCClassNode = {

    val cr = new ClassReader(is)
    val classNode = new ClassNode(ASM5)
    cr.accept(classNode, 0)

    adaptClass(classNode)
  }

  def adaptClass(cl: ClassNode): VBCClassNode = new VBCClassNode(
    cl.version,
    cl.access,
    cl.name,
    if (cl.signature == null) None else Some(cl.signature),
    cl.superName,
    if (cl.interfaces == null) Nil else cl.interfaces.toList,
    if (cl.fields == null) Nil else cl.fields.map(adaptField).toList,
    if (cl.methods == null) Nil else cl.methods.map(adaptMethod(cl.name, _)).toList,
    if (cl.sourceDebug != null && cl.sourceFile != null) Some(cl.sourceFile, cl.sourceDebug) else None,
    if (cl.outerClass != null) Some(cl.outerClass, cl.outerMethod, cl.outerMethodDesc) else None,
    if (cl.visibleAnnotations == null) Nil else cl.visibleAnnotations.toList,
    if (cl.invisibleAnnotations == null) Nil else cl.invisibleAnnotations.toList,
    if (cl.visibleTypeAnnotations == null) Nil else cl.visibleTypeAnnotations.toList,
    if (cl.invisibleTypeAnnotations == null) Nil else cl.invisibleTypeAnnotations.toList,
    if (cl.attrs == null) Nil else cl.attrs.toList,
    if (cl.innerClasses == null) Nil else cl.innerClasses.map(adaptInnerClass).toList
  )


  def adaptMethod(owner: String, m: MethodNode): VBCMethodNode = {
    //    println("\tMethod: " + m.name)
    val methodAnalyzer = new MethodAnalyzer(owner, m)
    methodAnalyzer.analyze()
    methodAnalyzer.validate()
    val ordered = methodAnalyzer.blocks.toArray :+ m.instructions.size()

    var varCache: Map[Int, Variable] = Map()
    //    if (m.parameters != null)
    //      for (paramIdx <- 0 until m.parameters.size())
    //        varCache += (paramIdx -> new Parameter(paramIdx, m.parameters(paramIdx).name))
    val isStatic = (m.access & Opcodes.ACC_STATIC) > 0
    val parameterCount = Type.getArgumentTypes(m.desc).size + (if (isStatic) 0 else 1)

    // adding "this" explicitly, because it may not be included if it's the only parameter
    if (!isStatic)
      varCache += (0 -> new Parameter(0, "this"))
    if (m.localVariables != null)
      for (i <- 0 until m.localVariables.size()) {
        val vIdx = m.localVariables(i).index
        if (vIdx < parameterCount)
          varCache += (vIdx -> new Parameter(vIdx, m.localVariables(i).name))
        else
          varCache += (vIdx -> new LocalVar(m.localVariables(i).name, m.localVariables(i).desc))
      }

    // typically we initialize all variables and parameters from the table, but that table is technically optional,
    // so we need a fallback option and generate them on the fly with name "$unknown"
    def lookupVariable(idx: Int): Variable =
      if (varCache contains idx)
        varCache(idx)
      else {
        val newVar = if (idx < parameterCount)
          new Parameter(idx, "$unknown")
        else
          new LocalVar("$unknown", "V")
        varCache += (idx -> newVar)
        newVar
      }

    def createBlock(start: Int, end: Int): Block = {
      val instrList = for (instrIdx <- start until end;
                           if m.instructions.get(instrIdx).getOpcode >= 0 || m.instructions.get(instrIdx).isInstanceOf[LineNumberNode])
        yield adaptBytecodeInstruction(m.instructions.get(instrIdx), methodAnalyzer.label2BlockIdx.apply, lookupVariable)
      new Block(instrList: _*)
    }



    val blocks = for (i <- 0 to ordered.length - 2)
      yield createBlock(ordered(i), ordered(i + 1))

    VBCMethodNode(
      m.access,
      m.name,
      m.desc,
      if (m.signature == null) None else Some(m.signature),
      if (m.exceptions == null) Nil else m.exceptions.toList,
      new CFG(blocks.toList),
      varCache.values.toList
    )
  }

  def adaptField(field: FieldNode): VBCFieldNode = new VBCFieldNode(
    field.access,
    field.name,
    field.desc,
    field.signature,
    field.value,
    if (field.visibleAnnotations == null) Nil else field.visibleAnnotations.toList,
    if (field.invisibleAnnotations == null) Nil else field.invisibleAnnotations.toList,
    if (field.visibleTypeAnnotations == null) Nil else field.visibleTypeAnnotations.toList,
    if (field.invisibleTypeAnnotations == null) Nil else field.invisibleTypeAnnotations.toList,
    if (field.attrs == null) Nil else field.attrs.toList
  )

  def adaptInnerClass(m: InnerClassNode): VBCInnerClassNode = new VBCInnerClassNode(
    m.name,
    m.outerName,
    m.innerName,
    m.access
  )


  def adaptBytecodeInstruction(inst: AbstractInsnNode, labelLookup: LabelNode => Int, variables: Int => Variable): Instruction =
    inst.getOpcode match {
      case NOP => UNKNOWN()
      case ACONST_NULL => InstrACONST_NULL()
      case ICONST_M1 => InstrICONST(-1)
      case ICONST_0 => InstrICONST(0)
      case ICONST_1 => InstrICONST(1)
      case ICONST_2 => InstrICONST(2)
      case ICONST_3 => InstrICONST(3)
      case ICONST_4 => InstrICONST(4)
      case ICONST_5 => InstrICONST(5)
      case LCONST_0 => UNKNOWN(LCONST_0)
      case LCONST_1 => UNKNOWN(LCONST_1)
      case FCONST_0 => UNKNOWN(FCONST_0)
      case FCONST_1 => UNKNOWN(FCONST_1)
      case FCONST_2 => UNKNOWN(FCONST_2)
      case DCONST_0 => UNKNOWN(DCONST_0)
      case DCONST_1 => UNKNOWN(DCONST_1)
      case BIPUSH => {
        val i = inst.asInstanceOf[IntInsnNode]
        InstrBIPUSH(i.operand)
      }
      case SIPUSH => {
        val i = inst.asInstanceOf[IntInsnNode]
        InstrSIPUSH(i.operand)
      }
      case LDC => {
        val i = inst.asInstanceOf[LdcInsnNode]
        InstrLDC(i.cst)
      }
      case ILOAD => {
        val i = inst.asInstanceOf[VarInsnNode]
        InstrILOAD(variables(i.`var`))
      }
      case LLOAD => UNKNOWN(LLOAD)
      case FLOAD => UNKNOWN(FLOAD)
      case DLOAD => UNKNOWN(DLOAD)
      case ALOAD => {
        val i = inst.asInstanceOf[VarInsnNode]
        InstrALOAD(variables(i.`var`))
      }
      case IALOAD => InstrIALOAD()
      case LALOAD => UNKNOWN(LALOAD)
      case FALOAD => UNKNOWN(FALOAD)
      case DALOAD => UNKNOWN(DALOAD)
      case AALOAD => InstrAALOAD()
      case BALOAD => UNKNOWN(BALOAD)
      case CALOAD => InstrCALOAD()
      case SALOAD => UNKNOWN(SALOAD)
      case ISTORE => {
        val i = inst.asInstanceOf[VarInsnNode]
        InstrISTORE(variables(i.`var`))
      }
      case LSTORE => UNKNOWN(LSTORE)
      case FSTORE => UNKNOWN(FSTORE)
      case DSTORE => UNKNOWN(DSTORE)
      case ASTORE => {
        val i = inst.asInstanceOf[VarInsnNode]
        InstrASTORE(variables(i.`var`))
      }
      case IASTORE => InstrIASTORE()
      case LASTORE => UNKNOWN(LASTORE)
      case FASTORE => UNKNOWN(FASTORE)
      case DASTORE => UNKNOWN(DASTORE)
      case AASTORE => InstrAASTORE()
      case BASTORE => UNKNOWN(BASTORE)
      case CASTORE => InstrCASTORE()
      case SASTORE => UNKNOWN(SASTORE)
      case POP => InstrPOP()
      case POP2 => UNKNOWN(POP2)
      case DUP => InstrDUP()
      case DUP_X1 => InstrDUP_X1()
      case DUP_X2 => UNKNOWN(DUP_X2)
      case DUP2 => UNKNOWN(DUP2)
      case DUP2_X1 => UNKNOWN(DUP2_X1)
      case DUP2_X2 => UNKNOWN(DUP2_X2)
      case SWAP => UNKNOWN(SWAP)
      case IADD => InstrIADD()
      case LADD => UNKNOWN(LADD)
      case FADD => UNKNOWN(FADD)
      case DADD => UNKNOWN(DADD)
      case ISUB => InstrISUB()
      case LSUB => UNKNOWN(LSUB)
      case FSUB => UNKNOWN(FSUB)
      case DSUB => UNKNOWN(DSUB)
      case IMUL => InstrIMUL()
      case LMUL => UNKNOWN(LMUL)
      case FMUL => UNKNOWN(FMUL)
      case DMUL => UNKNOWN(DMUL)
      case IDIV => InstrIDIV()
      case LDIV => UNKNOWN(LDIV)
      case FDIV => UNKNOWN(FDIV)
      case DDIV => UNKNOWN(DDIV)
      case IREM => UNKNOWN(IREM)
      case LREM => UNKNOWN(LREM)
      case FREM => UNKNOWN(FREM)
      case DREM => UNKNOWN(DREM)
      case INEG => InstrINEG()
      case LNEG => UNKNOWN(LNEG)
      case FNEG => UNKNOWN(FNEG)
      case DNEG => UNKNOWN(DNEG)
      case ISHL => UNKNOWN(ISHL)
      case LSHL => UNKNOWN(LSHL)
      case ISHR => UNKNOWN(ISHR)
      case LSHR => UNKNOWN(LSHR)
      case IUSHR => UNKNOWN(IUSHR)
      case LUSHR => UNKNOWN(LUSHR)
      case IAND => UNKNOWN(IAND)
      case LAND => UNKNOWN(LAND)
      case IOR => UNKNOWN(IOR)
      case LOR => UNKNOWN(LOR)
      case IXOR => UNKNOWN(IXOR)
      case LXOR => UNKNOWN(LXOR)
      case IINC => {
        val i = inst.asInstanceOf[IincInsnNode]
        InstrIINC(variables(i.`var`), i.incr)
      }
      case I2L => UNKNOWN(I2L)
      case I2F => UNKNOWN(I2F)
      case I2D => UNKNOWN(I2D)
      case L2I => UNKNOWN(L2I)
      case L2F => UNKNOWN(L2F)
      case L2D => UNKNOWN(L2D)
      case F2I => UNKNOWN(F2I)
      case F2L => UNKNOWN(F2L)
      case F2D => UNKNOWN(F2D)
      case D2I => UNKNOWN(D2I)
      case D2L => UNKNOWN(D2L)
      case D2F => UNKNOWN(D2F)
      case I2B => UNKNOWN(I2B)
      case I2C => InstrI2C()
      case I2S => UNKNOWN(I2S)
      case LCMP => UNKNOWN(LCMP)
      case FCMPL => UNKNOWN(FCMPL)
      case FCMPG => UNKNOWN(FCMPG)
      case DCMPL => UNKNOWN(DCMPL)
      case DCMPG => UNKNOWN(DCMPG)
      case IFEQ => {
        val insIFEQ = inst.asInstanceOf[JumpInsnNode]
        val label = insIFEQ.label
        InstrIFEQ(labelLookup(label))
      }
      case IFNE => {
        val i = inst.asInstanceOf[JumpInsnNode]
        val label = i.label
        InstrIFNE(labelLookup(label))
      }
      case IFLT => {
        val i = inst.asInstanceOf[JumpInsnNode]
        InstrIFLT(labelLookup(i.label))
      }
      case IFGE => {
        val i = inst.asInstanceOf[JumpInsnNode]
        InstrIFGE(labelLookup(i.label))
      }
      case IFGT => {
        val i = inst.asInstanceOf[JumpInsnNode]
        InstrIFGT(labelLookup(i.label))
      }
      case IFLE => UNKNOWN(IFLE)
      case IF_ICMPEQ => {
        val i = inst.asInstanceOf[JumpInsnNode]
        InstrIF_ICMPEQ(labelLookup(i.label))
      }
      case IF_ICMPNE => {
        val i = inst.asInstanceOf[JumpInsnNode]
        InstrIF_ICMPNE(labelLookup(i.label))
      }
      case IF_ICMPLT => {
        val i = inst.asInstanceOf[JumpInsnNode]
        InstrIF_ICMPLT(labelLookup(i.label))
      }
      case IF_ICMPGE => {
        val i = inst.asInstanceOf[JumpInsnNode]
        InstrIF_ICMPGE(labelLookup(i.label))
      }
      case IF_ICMPGT => UNKNOWN(IF_ICMPGT)
      case IF_ICMPLE => {
        val i = inst.asInstanceOf[JumpInsnNode]
        InstrIF_ICMPLE(labelLookup(i.label))
      }
      case GOTO => {
        val i = inst.asInstanceOf[JumpInsnNode]
        InstrGOTO(labelLookup(i.label))
      }
      case JSR => UNKNOWN(JSR)
      case RET => UNKNOWN(RET)
      case TABLESWITCH => UNKNOWN(TABLESWITCH)
      case LOOKUPSWITCH => UNKNOWN(LOOKUPSWITCH)
      case IRETURN => InstrIRETURN()
      case LRETURN => UNKNOWN(LRETURN)
      case FRETURN => UNKNOWN(FRETURN)
      case DRETURN => UNKNOWN(DRETURN)
      case ARETURN => InstrARETURN()
      case RETURN => InstrRETURN()
      case GETSTATIC => {
        val i = inst.asInstanceOf[FieldInsnNode]
        InstrGETSTATIC(i.owner, i.name, i.desc)
      }
      case PUTSTATIC => {
        val i = inst.asInstanceOf[FieldInsnNode]
        InstrPUTSTATIC(i.owner, i.name, i.desc)
      }
      case GETFIELD => {
        val i = inst.asInstanceOf[FieldInsnNode]
        InstrGETFIELD(i.owner, i.name, i.desc)
      }
      case PUTFIELD => {
        val i = inst.asInstanceOf[FieldInsnNode]
        InstrPUTFIELD(i.owner, i.name, i.desc)
      }
      case INVOKEVIRTUAL => {
        val i = inst.asInstanceOf[MethodInsnNode]
        InstrINVOKEVIRTUAL(i.owner, i.name, i.desc, i.itf)
      }
      case INVOKESPECIAL => {
        val i = inst.asInstanceOf[MethodInsnNode]
        InstrINVOKESPECIAL(i.owner, i.name, i.desc, i.itf)
      }
      case INVOKESTATIC => {
        val i = inst.asInstanceOf[MethodInsnNode]
        InstrINVOKESTATIC(i.owner, i.name, i.desc, i.itf)
      }
      case INVOKEINTERFACE => {
        val i = inst.asInstanceOf[MethodInsnNode]
        InstrINVOKEINTERFACE(i.owner, i.name, i.desc, i.itf)
      }
      case INVOKEDYNAMIC => UNKNOWN(INVOKEDYNAMIC)
      case NEW => {
        val i = inst.asInstanceOf[TypeInsnNode]
        InstrNEW(i.desc)
      }
      case NEWARRAY => {
        val i = inst.asInstanceOf[IntInsnNode]
        InstrNEWARRAY(i.operand)
      }
      case ANEWARRAY => {
        val i = inst.asInstanceOf[TypeInsnNode]
        InstrANEWARRAY(i.desc)
      }
      case ARRAYLENGTH => UNKNOWN(ARRAYLENGTH)
      case ATHROW => UNKNOWN(ATHROW)
      case CHECKCAST => {
        val i = inst.asInstanceOf[TypeInsnNode]
        InstrCHECKCAST(i.desc)
      }
      case INSTANCEOF => UNKNOWN(INSTANCEOF)
      case MONITORENTER => UNKNOWN(MONITORENTER)
      case MONITOREXIT => UNKNOWN(MONITOREXIT)
      case MULTIANEWARRAY => UNKNOWN(MULTIANEWARRAY)
      case IFNULL => {
        val i = inst.asInstanceOf[JumpInsnNode]
        InstrIFNULL(labelLookup(i.label))
      }
      case IFNONNULL => {
        val i = inst.asInstanceOf[JumpInsnNode]
        InstrIFNONNULL(labelLookup(i.label))
      }
      case -1 =>
        // special nodes in ASM such as LineNumberNode and LabelNode
        inst match {
          case ln: LineNumberNode => InstrLINENUMBER(ln.line)
          case _ => InstrNOP()
        }
      case _ => {
        UNKNOWN()
      }
    }


}