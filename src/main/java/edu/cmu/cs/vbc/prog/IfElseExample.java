package edu.cmu.cs.vbc.prog;

import edu.cmu.cs.varex.annotation.VConditional;

/**
 * @author: chupanw
 */
public class IfElseExample {
    @VConditional
    public boolean a;

    public static void main(String[] args) {
        new IfElseExample().simpleIfElse();
    }

    public void simpleIfElse() {
//        System.out.println(a);
        if (a) {
            System.out.println("then branch");
        }
        else {
            System.out.println("else branch");
        }
    }
}
