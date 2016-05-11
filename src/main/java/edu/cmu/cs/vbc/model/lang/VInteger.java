package edu.cmu.cs.vbc.model.lang;

import de.fosd.typechef.featureexpr.FeatureExpr;
import edu.cmu.cs.varex.V;
import edu.cmu.cs.varex.annotation.Immutable;

/**
 * @author chupanw
 */
@Immutable
public class VInteger {

    public Integer actual;

    public VInteger(int v, FeatureExpr ctx) {
        actual = v;
    }

    public VInteger(int v) {
        actual = v;
    }

    @Override
    public String toString() {
        return actual.toString();
    }

    public VString toString(FeatureExpr ctx) {
        return new VString(actual.toString());
    }

    public static VInteger valueOf(int v) {
        return new VInteger(v);
    }

    public static VInteger valueOf(int v, FeatureExpr ctx) {
        return new VInteger(v);
    }

    public int intValue() {
        return actual;
    }

    public VInteger intValue(FeatureExpr ctx) {
        return new VInteger(actual);
    }

    //////////////////////////////////////////////////
    // V methods
    //////////////////////////////////////////////////

    public V<? extends VInteger> compareTo$Ljava_lang_Integer$I(V<? extends VInteger> that, FeatureExpr ctx) {
        return that.map(x -> new VInteger(actual.compareTo(x.actual), ctx));
    }

    public static V<? extends VInteger> valueOf$I$Ljava_lang_Integer(V<? extends VInteger> v, FeatureExpr ctx) {
        return v;
    }

    public static V<? extends VInteger> parseInt$Ljava_lang_StringI$I(V<? extends VString> vs, V<? extends VInteger> vi, FeatureExpr ctx) {
        return vs.sflatMap(ctx, x -> {
            return vi.smap(ctx, y -> {
                return new VInteger(Integer.parseInt(x.actual, y.actual));
            });
        });
    }

    public static V<? extends VInteger> Vinit$I$V(V<? extends VInteger> vi, FeatureExpr ctx) {
        return vi.select(ctx);
    }
}
