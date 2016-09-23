package edu.cmu.cs.vbc.prog;

/**
 * @author chupanw
 */
public class StringBuilderExample {

    boolean A;

    private StringBuilder buf;

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
    }

}