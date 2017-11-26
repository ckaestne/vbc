package edu.cmu.cs.vbc;

public class C {

    boolean b() {
        return true;
    }

    public B foo() {
        B x;
        if (b()) {
            x = new B();
        } else x = new A();
        return x;


    }
}
