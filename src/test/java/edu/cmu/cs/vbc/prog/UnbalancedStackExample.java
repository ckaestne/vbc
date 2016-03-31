package edu.cmu.cs.vbc.prog;

import edu.cmu.cs.varex.annotation.VConditional;

/**
 * An example showing that stack may not be empty at the boundary of block
 *
 * @author chupanw
 */
public class UnbalancedStackExample {

    @VConditional
    boolean A;

    int a;

    public static void main(String[] args) {
        System.out.println(new UnbalancedStackExample().unbalancedIfElse());
    }

    /**
     * Initialized filed a conditionally, depending on field A
     */
    public UnbalancedStackExample() {
        if (A) {
            a = 1;
        } else {
            a = 2;
        }
    }

    /**
     * Complied bytecode looks like:
     * ALOAD 0
     * GETFIELD a
     * ICONST_1
     * IF_ICMPNE L0    <- end of block 0, empty stack
     * ICONST_1
     * GOTO L1         <- end of block 1, ONE element on stack
     * L0
     * ICONST_0        <- end of block 2, ONE element on stack
     * L1
     * IRETURN         <- beginning of block 3, should have ONE element of stack
     *
     * @return Choice<A, true, false>
     */
    public boolean unbalancedIfElse() {
        return a == 1;
    }
}
