package edu.cmu.cs.vbc.model.io;

import de.fosd.typechef.featureexpr.FeatureExpr;
import edu.cmu.cs.varex.V;
import edu.cmu.cs.varex.annotation.Immutable;
import edu.cmu.cs.vbc.model.lang.VInteger;
import edu.cmu.cs.vbc.model.lang.VString;

import java.io.PrintStream;

/**
 * @author chupanw
 */
@Immutable
public class VPrintStream {

    private PrintStream actual;

    public VPrintStream(PrintStream ps) {
        actual = ps;
    }

    //////////////////////////////////////////////////
    // Mirrored methods
    //////////////////////////////////////////////////

    public void println(VString s, FeatureExpr ctx) {
        actual.println(s);
    }

    public void print(VString s, FeatureExpr ctx) {
        actual.print(s);
    }

    //////////////////////////////////////////////////
    // V methods
    //////////////////////////////////////////////////

    public void println(V v, FeatureExpr ctx) {
        actual.println(v.select(ctx));
    }

    public void println$Ljava_lang_Object$V(V v, FeatureExpr ctx) {
        actual.println(v.select(ctx));
    }

    public void println$Ljava_lang_String$V(V<? extends VString> vs, FeatureExpr ctx) {
        actual.println(vs.select(ctx));
    }

    public void println$I$V(V<? extends VInteger> vi, FeatureExpr ctx) {
        actual.println(vi.select(ctx));
    }

    /**
     * Print V<VBoolean>. Internally, it's actually VInteger
     * @param vi
     * @param ctx
     */
    public void println$Z$V(V<? extends VInteger> vi, FeatureExpr ctx) {
        actual.println(vi.select(ctx));
    }

    public void println$C$V(V<? extends VInteger> vi, FeatureExpr ctx) {
        actual.println(vi.select(ctx));
    }
}
