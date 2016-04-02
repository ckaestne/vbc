package edu.cmu.cs.vbc.prog;

import edu.cmu.cs.varex.annotation.VConditional;

/**
 * test conditional writes to fields
 */
public class FieldTest {

    @VConditional
    public boolean A;
    @VConditional
    public boolean B;


    public static void main(String[] args) {
        new FieldTest().test0();
        new FieldTest().test1();
        new FieldTest().test2();
    }

    private void test0() {
        if (A && B)
            System.out.println("A&&B");
    }

    private void test1() {
        F1 object = new F1();
        object.i = 1;
        System.out.println(object.i);

        if (A)
            object.i = 5;
        System.out.println(object.i);

        if (A)
            object = new F1();
        System.out.println(object.i);

        if (A && B)
            object.i = 2;
        System.out.println(object.i);

        if (B)
            object.i = 3;
        System.out.println(object.i);
    }


    private void test2() {
        F1 o1 = new F1();
        o1.i = 100;
        F1 o2 = new F1();
        o2.i = 101;

        foo(o1.i, o2.i, null);

        F1 object = o1;
        object.i = 1;
        System.out.println(object.i);

        if (A)
            object.i = 5;
        foo(o1.i, o2.i, object.i);

        if (A)
            object = o2;
        foo(o1.i, o2.i, object.i);

        if (A && B)
            object.i = 2;
        foo(o1.i, o2.i, object.i);

        if (B)
            object.i = 3;
        foo(o1.i, o2.i, object.i);
    }

    private void foo(Integer i1, Integer i2, Integer i3) {
        System.out.println(i1);
        System.out.println(i2);
        System.out.println(i3);
        System.out.println("--");
    }

}

class F1 {
    Integer i = 0;
}