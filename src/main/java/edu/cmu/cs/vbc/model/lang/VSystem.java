package edu.cmu.cs.vbc.model.lang;

import edu.cmu.cs.varex.annotation.Immutable;
import edu.cmu.cs.vbc.model.io.VPrintStream;

import java.io.PrintStream;

/**
 * @author chupanw
 */
@Immutable
public class VSystem {

    /* No actual field because java.lang.System could not be initialized */

    public static VPrintStream out = new VPrintStream(System.out);
}
