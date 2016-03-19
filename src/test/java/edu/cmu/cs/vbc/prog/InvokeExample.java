package edu.cmu.cs.vbc.prog;

import edu.cmu.cs.varex.annotation.VConditional;
import edu.cmu.cs.vbc.model.VInteger;

/**
 * Toy example for method invocation.
 *
 * Method should be invoked on a {@link edu.cmu.cs.varex.V} object
 *
 * @author chupanw
 */
public class InvokeExample {

    @VConditional boolean A = false;

    VInteger a;

    /**
     * Initialize field a depending on Conditional A
     */
    public InvokeExample() {
        if (A) {
            a = new VInteger(1);
        }
        else {
            a = new VInteger(3);
        }
    }

    /**
     * Invoke compareTo() on field a
     *
     * @return Choice<A, -1, 1>
     */
    public Integer simpleInvoke() {
        VInteger b = new VInteger(2);
        return a.compareTo(b);
    }

    public static void main(String[] args) {
        System.out.println(new InvokeExample().simpleInvoke());
    }
}
