package edu.cmu.cs.vbc.prog.gpl;

import edu.cmu.cs.varex.annotation.VConditional;

import java.io.IOException;

public class Mini {

    private int xA = 0;
    private int xB = 0;

    public static void main(String[] args) throws IOException {
        new Mini().main();

    }

    private void main() {
        if (!valid()) return;

        System.out.println("A: " + A);
        if (A) xA++;
        System.out.println("B: " + B);
        if (B) xB++;
        System.out.println("valid: " + valid());
        System.out.println(xA + xB);
    }

    @VConditional
    public boolean A = true;
    @VConditional
    public boolean B = true;

    public boolean valid() {
        return A != B;
    }
}
