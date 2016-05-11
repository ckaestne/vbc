package edu.cmu.cs.varex;

import de.fosd.typechef.featureexpr.FeatureExpr;
import edu.cmu.cs.vbc.model.lang.VBoolean;
import edu.cmu.cs.vbc.model.lang.VInteger;

/**
 * Created by ckaestne on 1/16/2016.
 */
public class VOps {

    public static V<? extends VInteger> IADD(V<? extends VInteger> a, V<? extends VInteger> b) {
        return a.flatMap(aa -> {
            if (aa == null)
                return V.one(null);
            else
                return b.map(bb -> {
                    if (bb == null)
                        return null;
                    else
                        return new VInteger(aa.intValue() + bb.intValue());
                });
        });
    }

    public static V<? extends VInteger> IINC(V<? extends VInteger> a, int increment) {
        return a.map(aa -> {
            if (aa == null)
                return null;
            else {
                return new VInteger(aa.intValue() + increment);
            }
        });
    }

    /**
     * Called by lifted bytecode, compare with 0
     *
     * @param a
     * @return
     */
    public static FeatureExpr whenEQ(V a) {
        return a.when(v -> {
            if (v == null)
                return false;
            else {
                if (v instanceof VBoolean) {
                    return ((VBoolean) v).actual == false;
                }
                else if (v instanceof VInteger) {
                    return ((VInteger) v).intValue() == 0;
                }
                else {
                    throw new RuntimeException("Unsupported type in whenEQ");
                }
            }
        });
    }

    /**
     * Called by lifted bytecode, compare with 0
     *
     * @param a
     * @return
     */
    public static FeatureExpr whenNE(V a) {
        return a.when(v -> {
            if (v == null)
                return false;
            else {
                if (v instanceof VBoolean) {
                    return ((VBoolean) v).actual == true;
                } else if (v instanceof VInteger) {
                    return ((VInteger) v).intValue() != 0;
                }
                else {
                    throw new RuntimeException("Unsupported type in whenEQ");
                }
            }
        });
    }

    public static FeatureExpr whenGT(V<? extends VInteger> a) {
        return a.when(v -> v.intValue() > 0);
    }

    public static FeatureExpr whenGE(V<? extends VInteger> a) {
        return a.when(v -> {
            if (v == null)
                return false;
            else
                return v.intValue() >= 0;
        });
    }

    public static FeatureExpr whenLT(V<? extends VInteger> a) {
        return a.when(v -> {
            if (v == null)
                return false;
            else
                return v.intValue() < 0;
        });
    }

    public static FeatureExpr whenLE(V<? extends VInteger> a) {
        return a.when(v -> {
            if (v == null)
                return false;
            else
                return v.intValue() <= 0;
        });
    }

    public static FeatureExpr whenNONNULL(V<? extends Object> a) {
        return a.when(v -> v != null);
    }

    public static FeatureExpr whenNULL(V<? extends Object> a) {
        return a.when(v -> v == null);
    }

    public static FeatureExpr whenIEQ(V<? extends VInteger> a, V<? extends VInteger> b) {
        V<? extends VInteger> sub = ISUB(a, b);
        return whenEQ(sub);
    }

    public static FeatureExpr whenIGE(V<? extends VInteger> a, V<? extends VInteger> b) {
        V<? extends VInteger> sub = ISUB(a, b);
        return whenGE(sub);
    }

    public static FeatureExpr whenILT(V<? extends VInteger> a, V<? extends VInteger> b) {
        V<? extends VInteger> sub = ISUB(a, b);
        return whenLT(sub);
    }

    public static FeatureExpr whenILE(V<? extends VInteger> a, V<? extends VInteger> b) {
        V<? extends VInteger> sub = ISUB(a, b);
        return whenLE(sub);
    }

    public static FeatureExpr whenINE(V<? extends VInteger> a, V<? extends VInteger> b) {
        V<? extends VInteger> sub = ISUB(a, b);
        return whenNE(sub);
    }

    public static V<? extends VInteger> ISUB(V<? extends VInteger> a, V<? extends VInteger> b) {
        return a.flatMap(aa -> {
            if (aa == null)
                return V.one(null);
            else
                return b.map(bb -> {
                    if (bb == null)
                        return null;
                    else
                        return new VInteger(aa.intValue() - bb.intValue());
                });
        });
    }

    public static V<? extends VInteger> IMUL(V<? extends VInteger> a, V<? extends VInteger> b) {
        return a.flatMap(aa -> {
            if (aa == null)
                return V.one(null);
            else
                return b.map(bb -> {
                    if (bb == null)
                        return null;
                    else
                        return new VInteger(aa.intValue() * bb.intValue());
                });
        });
    }

    public static V<? extends VInteger> IDIV(V<? extends VInteger> a, V<? extends VInteger> b) {
        return a.flatMap(aa -> b.map(bb -> new VInteger(aa.intValue() / bb.intValue())));
    }

    public static V<? extends VInteger> i2c(V<? extends VInteger> a, FeatureExpr ctx) {
        return a.smap((v -> {
            int i = v.intValue();
            char c = (char)i;
            return new VInteger((int)c);
        }), ctx);
    }


}
