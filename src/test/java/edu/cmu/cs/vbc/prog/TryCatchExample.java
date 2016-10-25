package edu.cmu.cs.vbc.prog;

import edu.cmu.cs.varex.annotation.VConditional;

/**
 * @author chupanw
 */
public class TryCatchExample {

    @VConditional
    boolean A;
    @VConditional
    boolean B;

    void tryCatch1(Integer a, Integer b) {
        Integer aa = a;
        Integer bb = b;
        if (A) aa++;
        if (B) bb--;
        try {
            Integer result = aa / bb;
            System.out.println(result);
        } catch (ArithmeticException e) {
            System.out.println("divide by zero");
        }
    }

    void tryFinally(Integer a, Integer b) {
        Integer aa = a;
        Integer bb = b;
        if (A) aa++;
        if (B) bb--;
        try {
            Integer result = aa / bb;
            System.out.println(result);
        } finally {
            System.out.println("aa: " + aa);
            System.out.println("bb: " + bb);
        }
    }

    public static void main(String[] args) {
        TryCatchExample example = new TryCatchExample();
//        example.tryCatch1(1, 1);
        example.tryCatch1(1, 2);
        example.tryFinally(1, 2);
    }
}
