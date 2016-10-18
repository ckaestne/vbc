package edu.cmu.cs.vbc.prog;

import edu.cmu.cs.varex.annotation.VConditional;

/**
 * @author chupanw
 */
public class StringBuilderExample {

    @VConditional
    boolean A;

    public StringBuilder buf;

    public StringBuilderExample() {
        if (A) {
            buf = new StringBuilder("hi");
        } else {
            buf = new StringBuilder("hello");
        }
    }

    public void append(String s) {
        buf.append(s);
    }

    public static void main(String[] args) {
        StringBuilderExample a = new StringBuilderExample();
        a.append(" world");
        System.out.println(a.buf.toString());
    }

}