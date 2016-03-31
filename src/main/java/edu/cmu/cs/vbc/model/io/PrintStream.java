package edu.cmu.cs.vbc.model.io;

import de.fosd.typechef.featureexpr.FeatureExpr;
import edu.cmu.cs.varex.V;

/**
 * @author chupanw
 */
public class PrintStream {
    public static void println(java.io.PrintStream obj, V v, FeatureExpr fe) {
        obj.println(v);
    }
}
