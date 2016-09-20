package edu.cmu.cs.vbc.instr;

/**
 * @author chupanw
 */
public class Example_INSTANCEOF {
    public void foo(Object o) {
        if (o instanceof Integer) {
            System.out.println("o is Integer");
        } else {
            System.out.println("o is not Integer");
        }
    }
}
