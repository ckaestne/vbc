package edu.cmu.cs.vbc.prog;

import edu.cmu.cs.varex.annotation.VConditional;

/**
 * TODO: make this a test case
 * @author: chupanw
 */
public class IfElseExample {
    @VConditional
    public boolean a;

    public static void main(String[] args) {
        new IfElseExample().simpleIfElse();
    }

    public void simpleIfElse() {
        if (a) {
            System.out.println("then branch");
        }
        else {
            System.out.println("else branch");
        }
    }

//    /**
//     * previously only the last block may contain a return instruction
//     * @return
//     */
//    public int twoReturn() {
//        if (a) {
//            return 1;
//        }
//        else {
//            return 2;
//        }
//    }
}
