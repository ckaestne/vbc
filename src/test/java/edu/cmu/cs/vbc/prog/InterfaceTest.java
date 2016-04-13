package edu.cmu.cs.vbc.prog;

import edu.cmu.cs.varex.annotation.VConditional;

/**
 * @author chupanw
 */
public class InterfaceTest {

    @VConditional
    private boolean A;

    @VConditional
    private boolean B;

    public static void main(String[] args) {
        new InterfaceTest().test();
    }

    public void test() {
        FooInterface obj;
        if (A) {
            obj = new Foo1();
        }
        else {
            obj = new Foo2();
        }
        int value;
        if (B) {
            value = 1;
        }
        else {
            value = 10;
        }
        System.out.println(value);
        System.out.println(obj.increase(value));
        System.out.println(obj.decrease(value));
    }
}

class Foo1 implements FooInterface {

    @Override
    public int increase(int i) {
        return i + 1;
    }

    @Override
    public int decrease(int i) {
        return i - 1;
    }
}


class Foo2 implements FooInterface {

    @Override
    public int increase(int i) {
        return i + 2;
    }

    @Override
    public int decrease(int i) {
        return i - 2;
    }
}
