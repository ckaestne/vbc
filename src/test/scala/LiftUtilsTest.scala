package edu.cmu.cs.vbc

import edu.cmu.cs.vbc.utils.LiftUtils
import org.scalatest.{FunSuite, ShouldMatchers}


class LiftUtilsTest extends FunSuite with ShouldMatchers with DiffMethodTestInfrastructure {

  import edu.cmu.cs.vbc.utils.LiftUtils._

  def liftMethodDescription(s: String) = LiftUtils.liftMethodDescription(s, false)

  def liftMethodDescriptionC(s: String) = LiftUtils.liftMethodDescription(s, true)

  def liftMethodSignature(desc: String, sig: Option[String]) = LiftUtils.liftMethodSignature(desc, sig, false)

  def liftMethodSignatureC(desc: String, sig: Option[String]) = LiftUtils.liftMethodSignature(desc, sig, true)

  test("liftDesc") {
    liftMethodDescription("()V") should equal("(" + fexprclasstype + ")" + vclasstype)
    liftMethodDescription("(I)V") should equal("(" + vclasstype + fexprclasstype + ")" + vclasstype)
    liftMethodDescription("(II)V") should equal("(" + vclasstype + vclasstype + fexprclasstype + ")" + vclasstype)
    liftMethodDescription("(II)I") should equal("(" + vclasstype + vclasstype + fexprclasstype + ")" + vclasstype)
  }

  test("liftSignature") {
    liftMethodSignature("()V", None) should equal(None)
    liftMethodSignature("()V", Some("()V")) should equal(Some("(" + fexprclasstype + ")" + vclasstype))
    liftMethodSignature("", Some("()V")) should equal(Some("(" + fexprclasstype + ")" + vclasstype))
    liftMethodSignature("", Some("()I")) should equal(Some("(" + fexprclasstype + ")L" + vclassname + "<Ljava/lang/Integer;>;"))
    liftMethodSignature("(I)V", None) should equal(Some("(L" + vclassname + "<Ljava/lang/Integer;>;" + fexprclasstype + ")" + vclasstype))
    liftMethodSignature("", Some("(Ljava/lang/Object;)V")) should equal(Some("(L" + vclassname + "<Ljava/lang/Object;>;" + fexprclasstype + ")" + vclasstype))
    liftMethodSignature("", Some("(Ljava/lang/Object;Ljava/lang/Object;)V")) should equal(Some("(L" + vclassname + "<Ljava/lang/Object;>;L" + vclassname + "<Ljava/lang/Object;>;" + fexprclasstype + ")" + vclasstype))
    liftMethodSignature("", Some("([Ljava/lang/Object;)V")) should equal(Some("(L" + vclassname + "<[Ljava/lang/Object;>;" + fexprclasstype + ")" + vclasstype))
    liftMethodSignature("", Some("(Ljava/util/List<Ljava/lang/Object;>;)V")) should equal(Some("(L" + vclassname + "<Ljava/util/List<Ljava/lang/Object;>;>;" + fexprclasstype + ")" + vclasstype))
    liftMethodSignature("(I)I", None) should equal(Some("(L" + vclassname + "<Ljava/lang/Integer;>;" + fexprclasstype + ")L" + vclassname + "<Ljava/lang/Integer;>;"))
  }

  test("liftDescConstr") {
    liftMethodDescriptionC("()V") should equal("(" + fexprclasstype + ")V")
    liftMethodDescriptionC("(I)V") should equal("(" + vclasstype + fexprclasstype + ")V")
    liftMethodDescriptionC("(II)V") should equal("(" + vclasstype + vclasstype + fexprclasstype + ")V")
    liftMethodDescriptionC("(II)I") should equal("(" + vclasstype + vclasstype + fexprclasstype + ")" + vclasstype)
  }

  test("liftSignatureConstr") {
    liftMethodSignatureC("()V", None) should equal(None)
    liftMethodSignatureC("()V", Some("()V")) should equal(Some("(" + fexprclasstype + ")V"))
    liftMethodSignatureC("", Some("()V")) should equal(Some("(" + fexprclasstype + ")V"))
    liftMethodSignatureC("", Some("()I")) should equal(Some("(" + fexprclasstype + ")L" + vclassname + "<Ljava/lang/Integer;>;"))
    liftMethodSignatureC("(I)V", None) should equal(Some("(L" + vclassname + "<Ljava/lang/Integer;>;" + fexprclasstype + ")V"))
    liftMethodSignatureC("", Some("(Ljava/lang/Object;)V")) should equal(Some("(L" + vclassname + "<Ljava/lang/Object;>;" + fexprclasstype + ")V"))
    liftMethodSignatureC("", Some("(Ljava/lang/Object;Ljava/lang/Object;)V")) should equal(Some("(L" + vclassname + "<Ljava/lang/Object;>;L" + vclassname + "<Ljava/lang/Object;>;" + fexprclasstype + ")V"))
    liftMethodSignatureC("", Some("([Ljava/lang/Object;)V")) should equal(Some("(L" + vclassname + "<[Ljava/lang/Object;>;" + fexprclasstype + ")V"))
    liftMethodSignatureC("", Some("(Ljava/util/List<Ljava/lang/Object;>;)V")) should equal(Some("(L" + vclassname + "<Ljava/util/List<Ljava/lang/Object;>;>;" + fexprclasstype + ")V"))
    liftMethodSignatureC("(I)I", None) should equal(Some("(L" + vclassname + "<Ljava/lang/Integer;>;" + fexprclasstype + ")L" + vclassname + "<Ljava/lang/Integer;>;"))
  }

}