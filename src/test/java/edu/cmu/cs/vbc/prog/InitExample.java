package edu.cmu.cs.vbc.prog;

import edu.cmu.cs.varex.annotation.VConditional;

/**
 * @author chupanw
 */
public class InitExample {
    @VConditional
    boolean A;

    public static void main(String[] args) {
        InitExample example = new InitExample();
//        System.out.println(example.createSubClass().toString());
//        System.out.println(example.createSubClass2().toString());
        System.out.println(example.createSubClass3().value);
        System.out.println(example.createSubClass4().value);
    }

//    /**
//     * Simple case
//     */
//    public SubClass createSubClass() {
//        return new SubClass(1);
//    }
//
//    /**
//     * Create SubClass with conditional value
//     */
//    public SubClass createSubClass2() {
//        int a = 0;
//        if (A) {
//            a = 1;
//        } else {
//            a = 2;
//        }
//        return new SubClass(a);
//    }

    /**
     * Method call in super
     */
    public SubClass createSubClass3() {
        String str = "hello world";
        return new SubClass(str);
    }

    /**
     * Conditional method calls in super
     */
    public SubClass createSubClass4() {
        String str;
        if (A) {
            str = "hello world";
        } else {
            str = "hi world";
        }
        return new SubClass(str);
    }
}

class SubClass extends SuperClass {
//    SubClass(Integer i) {
//        super(i);
//    }

    SubClass(String s) {
        super(s.length() + 1);
    }

    @Override
    public String toString() {
        return value.toString();
    }
}

class SuperClass {
    protected Integer value;

    SuperClass(int v) {
        value = v;
    }
}
