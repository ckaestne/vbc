package edu.cmu.cs.vbc.model

import edu.cmu.cs.vbc.utils.LiftUtils._
import edu.cmu.cs.vbc.utils.LiftingPolicy
import edu.cmu.cs.vbc.vbytecode.{MethodDesc, MethodName, Owner, TypeDesc}
import org.objectweb.asm.Type

/**
  * Handle method calls, especially methods that we don't lift (e.g. java/lang/)
  *
  * @author chupanw
  */
object LiftCall {

  /**
    * Takes the method signature and returns the lifted method signature
    *
    * @param owner
    * @param name
    * @param desc
    * @return String -> lifted owner name,
    *         String -> lifted method name,
    *         String -> lifted method description
    *
    */
  def liftCall(
                owner: Owner,
                name: MethodName,
                desc: MethodDesc
              ): LiftedCall = {
    val shouldLiftMethod = LiftingPolicy.shouldLiftMethodCall(owner, name, desc)
    if (shouldLiftMethod) {
      /*
       * VarexC is going to lift this method
       *
       * owner: no need to change because VarexC is going to lift this method
       * name: same as above
       * desc: needs to be replaced with V, because this is the way VarexC lifts method signature
       * todo: could have type erasure problem
       */
      LiftedCall(
        LiftingPolicy.liftClassName(owner),
        name,
        replaceWithVs(desc, addFE = true),
        isLifting = true
      )
    }
    else {
      /*
      Methods that we shouldn't lift. For example, Integer.compareTo().
      There are two options at this point:
      1. if all arguments are non-V, we can just make the method call.
      2. if all arguments are V, we return the count of V arguments, and then use [[InvokeDynamicUtils]]
      to explode them. (Current design make sure that if there are V arguments, all arguments should be V)
       */
      LiftedCall(
        LiftingPolicy.liftClassName(owner),
        name,
        desc,
        isLifting = false
      )
    }
  }

  /** Scan and replace java library classes with model classes */
  private def replaceWithModel(desc: MethodDesc): MethodDesc = {
    val liftType: Type => String =
      (t: Type) => if (t == Type.VOID_TYPE) t.getDescriptor else LiftingPolicy.liftClassType(TypeDesc(t.toString))
    val mtype = Type.getMethodType(desc)
    MethodDesc(
      mtype.getArgumentTypes.map(liftType).mkString("(", "", ")") +
      liftType(Type.getType(primitiveToObjectType(mtype.getReturnType.toString)))
    )
  }

  /** Replace all the none-void parameter types with V types, also add FE to the end of parameter list */
  private def replaceWithVs(desc: MethodDesc, addFE: Boolean): MethodDesc = {
    val liftType: Type => String =
      (t: Type) => if (t == Type.VOID_TYPE) t.getDescriptor else "Ledu/cmu/cs/varex/V;"
    val mtype = Type.getMethodType(desc)
    MethodDesc(
      (mtype.getArgumentTypes.map(liftType) :+ s"${if (addFE) "Lde/fosd/typechef/featureexpr/FeatureExpr;" else ""}").
        mkString("(", "", ")") + liftType(mtype.getReturnType)
    )
  }

  def encodeTypeInName(mn: MethodName, desc: MethodDesc): MethodName = {
    mn.name match {
      case "<init>" => mn
      case _ => MethodName(mn.name + desc.replace('/', '_').
        replace('(', '$').
        replace(')', '$').
        replace(";", "").
        replace("[", "Array_"))
    }
  }

}

case class LiftedCall(
                       owner: Owner,
                       name: MethodName,
                       desc: MethodDesc,
                       isLifting: Boolean
                     )
