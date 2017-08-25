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

    /**
     * Minimal example from Checkstyle.
     *
     * In bytecode, there are three blocks that are protected by a TRYCATCHBLOCK. One of those leaves a
     * value on the operand stack while the other two leave the operand stack empty. This is problematic because
     * the incoming edges of catch block have incompatible height operand stack.
     */
    Integer tryWithIf() {
        try {
            Integer i = 3;
            if (i/0 == 4) {
                System.out.println("should not see this output");
            }
            return null;
        }
        catch (Exception e) {
            throw new RuntimeException();
        }
    }

    public static void main(String[] args) {
        TryCatchExample example = new TryCatchExample();
//        example.tryCatch1(1, 1);
        example.tryCatch1(1, 2);
        example.tryFinally(1, 2);
        example.tryWithIf();
    }
}
