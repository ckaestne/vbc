package edu.cmu.cs.vbc.utils

import edu.cmu.cs.vbc.ModelConfig
import edu.cmu.cs.vbc.vbytecode._

/**
  * Define lifting policy for methods and fields.
  *
  * In general, all classes in JDK should be lifted automatically. However, there are some
  * cases where default lifting is inefficient and we might need model classes. This object
  * defines which classes/methods/fields in JDK should be substituted, and which substitution
  * should we use.
  *
  * @todo Specify policies in configuration files or use drop-in.
  */
object LiftingPolicy {

  private var currentConfig: ModelConfig = ModelConfig.defaultConfig
  private[vbc] def setConfig(_config: ModelConfig) = currentConfig = _config
  def getConfig: ModelConfig = currentConfig

  /**
    * Return true if we lift this class
    *
    * By default, all JDK classes are copied as model classes, except for a few that we do not
    * want to change their names. For modeled JDK classes, we need to decided which of them
    * should be lifted. Reasons for lifting JDK classes include, but not limited to:
    *
    * - program classes extend JDK classes
    *
    * @param owner  The actual class name that will appear in the lifted bytecode.
    *               If this is a JDK class, it will be prefixed with "model".
    */
  def shouldLiftClass(owner: Owner): Boolean = {
    if (currentConfig.jdkLiftingClasses.exists(n => owner.name.matches(".*" + n))) return true
    if (currentConfig.libraryLiftingClasses.exists(n => owner.name.matches(".*" + n))) true
    else if (owner.name.startsWith("edu/cmu/cs/vbc/prog/") && !currentConfig.programNotLiftingClasses.exists(n => owner.name.matches(".*" + n))) true
    else false
  }

  /**
    * Return true if we want to life this method call
    *
    * The parameter ``owner`` might be renamed to model classes later.
    */
  def shouldLiftMethodCall(owner: Owner, name: MethodName, desc: MethodDesc): Boolean = {
    if (currentConfig.notLiftingClasses.exists(n => owner.name.matches(n))) false
    else true
  }

  /**
    * Return true if we want to lift this field
    *
    * @todo return the actual model class name if we decided to lift this method call.
    */
  def shouldLiftField(owner: Owner, name: FieldName, desc: TypeDesc): Boolean = {
    (owner, name, desc) match {
      case (Owner("java/lang/System"), FieldName("out"), _) => false
      case (Owner("java/lang/System"), FieldName("err"), _) => false
      case (Owner("java/util/Locale"), FieldName("GERMAN"), _) => false
      case (Owner("java/lang/Boolean"), FieldName("TYPE"), _) => false
      case (Owner("java/lang/Byte"), FieldName("TYPE"), _) => false
      case (Owner("java/lang/Integer"), FieldName("TYPE"), _) => false
      case (Owner("java/lang/Character"), FieldName("TYPE"), _) => false
      case (Owner("java/lang/Float"), FieldName("TYPE"), _) => false
      case (Owner("java/lang/Double"), FieldName("TYPE"), _) => false
      case (Owner("java/lang/Long"), FieldName("TYPE"), _) => false
      case (Owner("java/lang/Short"), FieldName("TYPE"), _) => false
      case (Owner("edu/cmu/cs/vbc/prog/checkstyle/api/SeverityLevel"), FieldName("ERROR"), _) => false
      case (Owner("edu/cmu/cs/vbc/prog/checkstyle/api/SeverityLevel"), FieldName("INFO"), _) => false
      case (Owner("java/nio/charset/CodingErrorAction"), FieldName("REPLACE"), _) => false
      case (Owner("edu/cmu/cs/vbc/prog/checkstyle/checks/LineSeparatorOption"), FieldName("SYSTEM"), _) => false
      case (Owner("java/math/BigInteger"), FieldName("ONE"), _) => false
      case (Owner("java/math/BigInteger"), FieldName("ZERO"), _) => false
      case (Owner("edu/cmu/cs/vbc/prog/checkstyle/checks/whitespace/PadOption"), FieldName("NOSPACE"), _) => false
      case (Owner("edu/cmu/cs/vbc/prog/checkstyle/checks/blocks/BlockOption"), FieldName("STMT"), _) => false
      case (Owner("edu/cmu/cs/vbc/prog/checkstyle/checks/blocks/LeftCurlyOption"), _, _) => false
      case (Owner("edu/cmu/cs/vbc/prog/checkstyle/checks/blocks/RightCurlyOption"), FieldName("SAME"), _) => false
      case (Owner("edu/cmu/cs/vbc/prog/checkstyle/api/Scope"), _, _) => false
      case (Owner("edu/cmu/cs/vbc/prog/checkstyle/checks/whitespace/WrapOption"), _, _) => false
      case (Owner("edu/cmu/cs/vbc/prog/checkstyle/checks/annotation/AnnotationUseStyleCheck$ElementStyle"), FieldName("COMPACT_NO_ARRAY"), _) => false
      case (Owner("edu/cmu/cs/vbc/prog/checkstyle/checks/annotation/AnnotationUseStyleCheck$TrailingArrayComma"), FieldName("NEVER"), _) => false
      case (Owner("edu/cmu/cs/vbc/prog/checkstyle/checks/annotation/AnnotationUseStyleCheck$ClosingParens"), FieldName("NEVER"), _) => false
      case (Owner("edu/cmu/cs/vbc/prog/checkstyle/checks/imports/ImportOrderOption"), FieldName("UNDER"), _) => false
      case (Owner("antlr/TokenStreamRecognitionException"), _, _) => false
      case (Owner("edu/cmu/cs/vbc/prog/checkstyle/TreeWalker$AstState"), _, _) => false
      case (Owner("java/io/File"), _, _) => false
      case _ => true
    }
  }

  case class LiftedCall(owner: Owner, name: MethodName, desc: MethodDesc, isLifting: Boolean)

  def liftCall(owner: Owner, name: MethodName, desc: MethodDesc): LiftedCall = {
    val shouldLiftMethod = LiftingPolicy.shouldLiftMethodCall(owner, name, desc)
    if (shouldLiftMethod) {
      if (name.contentEquals("<init>")) {
        // cpwtodo: handle exception in <init>
        LiftedCall(owner.toModel, name, desc.toVs_AppendFE_AppendArgs, isLifting = true)
      } else {
        LiftedCall(owner.toModel, name.rename(desc.toModels), desc.toVs.appendFE.toVReturnType, isLifting = true)
      }
    }
    else {
      replaceCall(owner, name, desc, isVE = true)
    }
  }

  def replaceCall(owner: Owner, name: MethodName, desc: MethodDesc, isVE: Boolean): LiftedCall = {
    // Although we are not lifting this method call, we might replace this call with our specialized
    // call because:
    //  (1) avoid native
    //  (2) activate our array expanding by changing the signature of System.arraycopy.
    //  (3) package access method
    (owner.name, name.name, desc.descString) match {
      case ("java/lang/System", "arraycopy", _) if isVE =>
        // We need array argument type in order to trigger array expansions and compressions.
        LiftedCall(Owner(VBCModel.prefix + "/java/lang/System"), name, MethodDesc(s"([${TypeDesc.getObject}I[${TypeDesc.getObject}II)V"), isLifting = false)
      case ("java/lang/Integer", "stringSize", _) =>
        LiftedCall(Owner(VBCModel.prefix + "/java/lang/Integer"), name, desc, isLifting = false)
      case ("java/lang/Integer", "getChars", _) =>
        LiftedCall(Owner(VBCModel.prefix + "/java/lang/Integer"), name, desc, isLifting = false)
      case (o, _, _) if o.startsWith("[") && isVE =>
        LiftedCall(Owner(s"[Ledu/cmu/cs/varex/V;"), name, desc, isLifting = false)
      case _ => LiftedCall(owner.toModel, name, desc.toModels, isLifting = false)
    }
  }
}
