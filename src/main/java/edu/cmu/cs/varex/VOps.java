package edu.cmu.cs.varex;

import de.fosd.typechef.featureexpr.FeatureExpr;

/**
 * Created by ckaestne on 1/16/2016.
 */
public class VOps {

    public static V<? extends Integer> IADD(V<? extends Integer> a, V<? extends Integer> b) {
        return a.flatMap((c, aa) -> {
            if (aa == null)
                return V.one(c, null);
            else
                return b.map(bb -> {
                    if (bb == null)
                        return null;
                    else
                        return aa + bb;
                });
        });
    }

    public static V<? extends Integer> IINC(V<? extends Integer> a, int increment) {
        return a.map(aa -> aa + increment);
    }

    /**
     * Called by lifted bytecode, compare with 0
     *
     * @param a
     * @return
     */
    public static FeatureExpr whenEQ(V<? extends Integer> a) {
        return a.when(v -> {
            if (v == null)
                return false;
            else
                return v == 0;
        });
    }

    /**
     * Called by lifted bytecode, compare with 0
     *
     * @param a
     * @return
     */
    public static FeatureExpr whenNE(V<? extends Integer> a) {
        return a.when(v -> {
            if (v == null)
                return false;
            else
                return v != 0;
        });
    }

    public static FeatureExpr whenGT(V<? extends Integer> a) {
        return a.when(v -> v > 0);
    }

    public static FeatureExpr whenGE(V<? extends Integer> a) {
        return a.when(v -> {
            if (v == null)
                return false;
            else
                return v >= 0;
        });
    }

    public static FeatureExpr whenLT(V<? extends Integer> a) {
        return a.when(v -> {
            if (v == null)
                return false;
            else
                return v < 0;
        });
    }

    public static FeatureExpr whenIEQ(V<? extends Integer> a, V<? extends Integer> b) {
        V<? extends Integer> sub = ISUB(a, b);
        return whenEQ(sub);
    }

    public static FeatureExpr whenIGE(V<? extends Integer> a, V<? extends Integer> b) {
        V<? extends Integer> sub = ISUB(a, b);
        return whenGE(sub);
    }

    public static FeatureExpr whenILT(V<? extends Integer> a, V<? extends Integer> b) {
        V<? extends Integer> sub = ISUB(a, b);
        return whenLT(sub);
    }

    public static FeatureExpr whenINE(V<? extends Integer> a, V<? extends Integer> b) {
        V<? extends Integer> sub = ISUB(a, b);
        return whenNE(sub);
    }

    public static V<? extends Integer> ISUB(V<? extends Integer> a, V<? extends Integer> b) {
        return a.flatMap((c, aa) -> {
            if (aa == null)
                return V.one(c, null);
            else
                return b.map(bb -> {
                    if (bb == null)
                        return null;
                    else
                        return aa - bb;
                });
        });
    }

    public static V<? extends Integer> IMUL(V<? extends Integer> a, V<? extends Integer> b) {
        return a.flatMap((c, aa) -> {
            if (aa == null)
                return V.one(c, null);
            else
                return b.map(bb -> {
                    if (bb == null)
                        return null;
                    else
                        return aa * bb;
                });
        });
    }

    public static V<? extends Integer> IDIV(V<? extends Integer> a, V<? extends Integer> b) {
        return a.flatMap(aa -> b.map(bb -> aa / bb));
    }

    public static void assertEquivalent(FeatureExpr expected, FeatureExpr found) {
        assert expected.equivalentTo(found) : String.format("expected %s, but found %s", expected, found);
    }


}
