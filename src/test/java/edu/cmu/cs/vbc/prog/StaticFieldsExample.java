package edu.cmu.cs.vbc.prog;

import edu.cmu.cs.varex.annotation.VConditional;

/**
 * @author: chupanw
 */

public class StaticFieldsExample {
    @VConditional
    public static boolean A;
    @VConditional
    public static boolean B;

    public static void main(String[] args) {
        System.out.println("start.");

        if (A)
            System.out.println("A");
        else
            System.out.println("!A");
        if (B)
            System.out.println("B");
        else
            System.out.println("!B");

        System.out.println("end.");
    }
}

