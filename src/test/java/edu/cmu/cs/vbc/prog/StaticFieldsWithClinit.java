package edu.cmu.cs.vbc.prog;

import edu.cmu.cs.varex.annotation.VConditional;

/**
 * @author: chupanw
 */

public class StaticFieldsWithClinit {
    @VConditional
    public static boolean A;
    @VConditional
    public static boolean B;

    // this would give us a <clinit> method
    // check whether our instrumentation could work with existing <clinit>
    public static boolean C = false;

    public static void main(String[] args) {
        StaticFieldsWithClinit example = new StaticFieldsWithClinit();
        example.test();
    }

    public void test() {
        System.out.println("start.");

        if (A)
            System.out.println("A");
        else
            System.out.println("!A");
        if (B)
            System.out.println("B");
        else
            System.out.println("!B");
        if (C)
            System.out.println("C");
        else
            System.out.println("!C");

        System.out.println("end.");
    }
}


