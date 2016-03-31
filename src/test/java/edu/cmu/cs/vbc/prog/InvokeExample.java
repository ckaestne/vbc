package edu.cmu.cs.vbc.prog;

import edu.cmu.cs.varex.annotation.VConditional;

/**
 * Toy example for method invocation.
 *
 * Method should be invoked on a {@link edu.cmu.cs.varex.V} object
 *
 * @author chupanw
 */
public class InvokeExample {

    @VConditional
    boolean A;

    Integer a;

    /**
     * Initialize field a depending on Conditional A
     */
    public InvokeExample() {
        if (A) {
            a = new Integer(1);
        }
        else {
            a = new Integer(3);
        }
    }

    /**
     * Invoke compareTo() on field a
     *
     * @return Choice<A, -1, 1>
     */
    public Integer simpleInvoke() {
        Integer b = new Integer(2);
        return a.compareTo(b);
    }

    public static void main(String[] args) {
        System.out.println(new InvokeExample().simpleInvoke());
    }
}
