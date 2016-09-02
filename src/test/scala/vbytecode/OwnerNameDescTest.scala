package vbytecode

import edu.cmu.cs.vbc.vbytecode._
import org.scalatest.FlatSpec

/**
  * @author chupanw
  */
class OwnerNameDescTest extends FlatSpec {

  "Owner" should "be a valid class name" in {
    Owner("java.lang.Object")
  }

  it should "throw exception if it starts with number" in {
    assertThrows[IllegalArgumentException] {
      Owner("1java")
    }
  }

  it should "throw exception if it has illegal character" in {
    assertThrows[IllegalArgumentException] {
      Owner("java&")
    }
  }

  "MethodName" should "be a valid Java identifier" in {
    MethodName("hello")
  }

  it should "throw exception if it is not a valid identifier" in {
    assertThrows[IllegalArgumentException] {
      MethodName("hello&")
    }
  }

  "FieldName" should "be a valid Java identifier" in {
    FieldName("foo")
  }

  it should "throw exception if it is not a valid identifier" in {
    assertThrows[IllegalArgumentException] {
      FieldName("bar%")
    }
  }

  "FieldDesc" can "be one of the eight primitive types" in {
    FieldDesc("Z") // boolean
    FieldDesc("C") // char
    FieldDesc("B") // byte
    FieldDesc("S") // short
    FieldDesc("I") // int
    FieldDesc("F") // float
    FieldDesc("J") // long
    FieldDesc("D") // double

    assertThrows[IllegalArgumentException] {
      FieldDesc("A")
    }
  }

  it can "be a valid class type" in {
    FieldDesc("Ljava/lang/Object;")
    assertThrows[IllegalArgumentException] {
      FieldDesc("Ljava/lang/Object")
      FieldDesc("java/lang/Object;")
      FieldDesc("java/lang/Object")
      FieldDesc("java.lang.Object")
    }
  }

  it can "be array" in {
    FieldDesc("[I")
    FieldDesc("[Ljava/lang/Object;")
    assertThrows[IllegalArgumentException] {
      FieldDesc("]I")
      FieldDesc("[A")
    }
  }

  "MethodDesc" should "has a list of type descriptors inside () followed by the return type" in {
    MethodDesc("(IF)V") // void m(int i, float f)
    MethodDesc("(Ljava/lang/Object;)I") // int m(Object o)
    MethodDesc("(ILjava/lang/String;)[I") // int[] m(int i, String s)
    MethodDesc("([I)Ljava/lang/Object;") // Object m(int[] i)
    MethodDesc("()V") // void m()

    assertThrows[IllegalArgumentException] {
      MethodDesc("IF)V") // parenthesis mismatch
      MethodDesc("(IFV") // parenthesis mismatch
      MethodDesc("(IF)A") // A is not a valid type
    }
  }
}
