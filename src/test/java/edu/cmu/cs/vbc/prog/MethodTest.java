package edu.cmu.cs.vbc.prog;

import edu.cmu.cs.varex.annotation.VConditional;

/**
 * testing conditional method calls
 */
public class MethodTest {

    @VConditional
    public boolean A;
    @VConditional
    public boolean B;
    @VConditional
    public boolean C;


    public static void main(String[] args) {
        new MethodTest().test1();
    }

    private void test1() {
        if (A)
            new F2().foo();

        F2 o1 = new F2();
        F2 o2 = new F2();
        F2 object = o1;
        if (B)
            object = o2;
        object.foo();
        o1.foo();

        String x = "1";
        if (C)
            x = "2";
        object.bar(x);
        if (A)
            object.bar(x);
        if (B)
            object.bar(x);
    }

}

class F2 {

    public void foo() {
        System.out.println("foo");
    }

    public void bar(String s) {
        System.out.print("bar:");
        System.out.println(s);
    }
}