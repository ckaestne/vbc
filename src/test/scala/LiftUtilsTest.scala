package edu.cmu.cs.vbc

import edu.cmu.cs.vbc.vbytecode.util.LiftUtils
import org.scalatest.{FunSuite, ShouldMatchers}


class LiftUtilsTest extends FunSuite with ShouldMatchers with DiffTestInfrastructure with LiftUtils {

    test("liftDesc") {
        liftMethodDescription("()V") should equal("(" + fexprclasstype + ")V")
        liftMethodDescription("(I)V") should equal("(" + vclasstype + fexprclasstype + ")V")
        liftMethodDescription("(II)V") should equal("(" + vclasstype + vclasstype + fexprclasstype + ")V")
        liftMethodDescription("(II)I") should equal("(" + vclasstype + vclasstype + fexprclasstype + ")" + vclasstype)
    }

    test("liftSignature") {
        liftMethodSignature("()V", None) should equal(None)
        liftMethodSignature("()V", Some("()V")) should equal(Some("(" + fexprclasstype + ")V"))
        liftMethodSignature("", Some("()V")) should equal(Some("(" + fexprclasstype + ")V"))
        liftMethodSignature("", Some("()I")) should equal(Some("(" + fexprclasstype + ")L" + vclassname + "<Ljava/lang/Integer;>;"))
        liftMethodSignature("(I)V", None) should equal(Some("(L" + vclassname + "<Ljava/lang/Integer;>;" + fexprclasstype + ")V"))
        liftMethodSignature("", Some("(Ljava/lang/Object;)V")) should equal(Some("(L" + vclassname + "<Ljava/lang/Object;>;" + fexprclasstype + ")V"))
        liftMethodSignature("", Some("(Ljava/lang/Object;Ljava/lang/Object;)V")) should equal(Some("(L" + vclassname + "<Ljava/lang/Object;>;L" + vclassname + "<Ljava/lang/Object;>;" + fexprclasstype + ")V"))
        liftMethodSignature("", Some("([Ljava/lang/Object;)V")) should equal(Some("(L" + vclassname + "<[Ljava/lang/Object;>;" + fexprclasstype + ")V"))
        liftMethodSignature("", Some("(Ljava/util/List<Ljava/lang/Object;>;)V")) should equal(Some("(L" + vclassname + "<Ljava/util/List<Ljava/lang/Object;>;>;" + fexprclasstype + ")V"))
        liftMethodSignature("(I)I", None) should equal(Some("(L" + vclassname + "<Ljava/lang/Integer;>;" + fexprclasstype + ")L" + vclassname + "<Ljava/lang/Integer;>;"))
    }

}