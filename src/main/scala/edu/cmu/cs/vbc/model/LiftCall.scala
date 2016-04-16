package edu.cmu.cs.vbc.model

import edu.cmu.cs.vbc.utils.LiftingFilter
import org.objectweb.asm.Type


/**
  * Handle method calls, especially methods that we don't lift (e.g. java/lang/)
  *
  * There are two cases to consider:
  *
  * 1. passing a V as argument actually makes sense (e.g. StringBuilder.append)
  * 2. passing a V as argument is not necessary (e.g. Integer.valueOf)
  *
  * For both cases, we use model classes. But 2 is easier because we only need to call (flat)map
  * on each V arguments.
  *
  * @author chupanw
  */
object LiftCall {

  /**
    * Takes the method signature and returns the lifted method signature
    *
    * @param hasVArguments We assume that either all arguments are Vs, or all arguments are not Vs.
    * @param owner
    * @param name
    * @param desc
    * @return (
    *         Boolean -> whether or not we are using model classes,
    *         Boolean -> whether or not we are invoking a lifted method (first boolean return value implies this one),
    *         String -> lifted owner name,
    *         String -> lifted method name,
    *         String -> lifted method description
    *         )
    */
  def liftCall(hasVArguments: Boolean, owner: String, name: String, desc: String, isStatic: Boolean, isConstructor: Boolean): (Boolean, Boolean, String, String, String) = {
    val shouldLiftMethod = LiftingFilter.shouldLiftMethod(owner, name, desc)
    if (shouldLiftMethod) {
      /*
       * false -> not invoking model classes (because this method should be lifted anyway)
       * true -> invoking a lifted method (which means we need to load ctx parameter to stack and no need to wrap return value into V)
       */
      return (false, true, owner, name, liftDesc(owner, desc, isStatic, isConstructor, false))
    }
    else if (hasVArguments) {
      /*
       * Now all arguments are Vs. There should be a model class for this
       */
      (true, true, getModelOwner(owner), name, liftDesc(owner, desc, isStatic, isConstructor, true))
    }
    else {
      /*
       * No V arguments at all, we are happy
       */
      (false, false, owner, name, desc)
    }
  }

  /**
    * Change owner to our own model class name
    *
    * @param owner For now assume we are only lifting classes in java/xxx
    * @return model class name
    */
  def getModelOwner(owner: String) = {
    assert(owner.startsWith("java/"))
    "edu/cmu/cs/vbc/model/" + owner.substring(5)
  }

  private def liftDesc(owner: String, desc: String, isStatic: Boolean, isConstructor: Boolean, needsModelClass: Boolean): String = {
    val liftType = (t: Type) => if (t == Type.VOID_TYPE && isConstructor) t.getDescriptor else "Ledu/cmu/cs/varex/V;"
    val mtype = Type.getMethodType(desc)
    "(" +
      (if (needsModelClass && !isStatic) Type.getObjectType(owner).getDescriptor else "") +
      (mtype.getArgumentTypes.map(liftType) :+ "Lde/fosd/typechef/featureexpr/FeatureExpr;").mkString("", "", ")") +
      liftType(mtype.getReturnType)
  }

}
