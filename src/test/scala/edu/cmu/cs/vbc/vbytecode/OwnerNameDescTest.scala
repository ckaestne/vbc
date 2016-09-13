package edu.cmu.cs.vbc.vbytecode

import org.scalatest.FlatSpec

/**
  * @author chupanw
  */
class OwnerNameDescTest extends FlatSpec {

  def expectString(s: String) {}

  "Owner" should "be a valid internal name" in {
    Owner("java/lang/Object")
  }

  it should "contain '/' instead of '.'" in {
    assertThrows[IllegalArgumentException] {
      Owner("java.lang.Object")
    }
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

  it can "be implicitly converted to String" in {
    expectString(Owner("java/lang/Object"))
  }

  it can "be primitive array" in {
    Owner("[I")
  }

  it can "be object array" in {
    Owner("[Ljava/lang/Integer;")
  }

  it can "be multidimensional primitive array" in {
    Owner("[[I")
    Owner("[[[I")
  }

  it can "be multidimensional object array" in {
    Owner("[[Ljava/lang/Integer;")
    Owner("[[[Ljava/lang/Integer;")
  }

  it can "be transformed to a valid TypeDesc" in {
    assert(Owner("java/lang/Object").getTypeDesc == TypeDesc("Ljava/lang/Object;"))
  }

  "MethodName" should "be a valid Java identifier" in {
    MethodName("hello")
  }

  it should "throw exception if it is not a valid identifier" in {
    assertThrows[IllegalArgumentException] {
      MethodName("hello&")
    }
  }

  it can "be <init> or <clinit>" in {
    MethodName("<init>")
    MethodName("<clinit>")
  }

  it can "be implicitly converted to String" in {
    expectString(MethodName("<init>"))
  }

  "FieldName" should "be a valid Java identifier" in {
    FieldName("foo")
  }

  it should "throw exception if it is not a valid identifier" in {
    assertThrows[IllegalArgumentException] {
      FieldName("bar%")
    }
  }

  it can "be implicitly converted to String" in {
    expectString(FieldName("hello"))
  }

  "TypeDesc" can "be one of the eight primitive types" in {
    TypeDesc("Z") // boolean
    TypeDesc("C") // char
    TypeDesc("B") // byte
    TypeDesc("S") // short
    TypeDesc("I") // int
    TypeDesc("F") // float
    TypeDesc("J") // long
    TypeDesc("D") // double

    assertThrows[IllegalArgumentException] {
      TypeDesc("A")
    }
  }

  it can "be a valid class type" in {
    TypeDesc("Ljava/lang/Object;")
    assertThrows[IllegalArgumentException] {
      TypeDesc("Ljava/lang/Object")
      TypeDesc("java/lang/Object;")
      TypeDesc("java/lang/Object")
      TypeDesc("java.lang.Object")
    }
  }

  it can "be array" in {
    TypeDesc("[I")
    TypeDesc("[Ljava/lang/Object;")
    assertThrows[IllegalArgumentException] {
      TypeDesc("]I")
      TypeDesc("[A")
    }
  }

  it can "not be void" in {
    assertThrows[IllegalArgumentException] {
      TypeDesc("V")
    }
  }

  it can "be implicitly converted to String" in {
    expectString(TypeDesc("I"))
  }

  "MethodDesc" should "has a list of type descriptors inside () followed by the return type" in {
    MethodDesc("(IF)V") // void m(int i, float f)
    MethodDesc("(Ljava/lang/Object;)I") // int m(Object o)
    MethodDesc("(ILjava/lang/String;)[I") // int[] m(int i, String s)
    MethodDesc("([I)Ljava/lang/Object;") // Object m(int[] i)

    assertThrows[IllegalArgumentException] {
      MethodDesc("IF)V") // parenthesis mismatch
      MethodDesc("(IFV") // parenthesis mismatch
      MethodDesc("(IF)A") // A is not a valid type
    }
  }

  it can "have empty parameter list and void return type" in {
    MethodDesc("()V") // void m()
  }

  it can "be implicitly converted to String" in {
    expectString(MethodDesc("()V"))
  }

  it can "return the number of arguments" in {
    assert(MethodDesc("(IF)V").getArgCount == 2)
    assert(MethodDesc("(Ljava/lang/Object;)I").getArgCount == 1)
    assert(MethodDesc("(ILjava/lang/String;)[I").getArgCount == 2)
    assert(MethodDesc("([I)Ljava/lang/Object;").getArgCount == 1)
  }

  it can "return the return type in String format" in {
    assert(MethodDesc("(IF)V").getReturnTypeString == "V")
    assert(MethodDesc("(Ljava/lang/Object;)I").getReturnTypeString == "I")
    assert(MethodDesc("(ILjava/lang/String;)[I").getReturnTypeString == "[I")
    assert(MethodDesc("([I)Ljava/lang/Object;").getReturnTypeString == "Ljava/lang/Object;")
  }
}
