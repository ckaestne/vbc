package edu.cmu.cs.varex;

import de.fosd.typechef.featureexpr.FeatureExpr;

/**
 * Created by ckaestne on 1/16/2016.
 */
public class VOps {

    public static V<? extends Integer> IADD(V<? extends Integer> a, V<? extends Integer> b, FeatureExpr ctx) {
        return a.sflatMap(ctx, (fe, aa) -> b.smap(fe, bb -> aa.intValue() + bb.intValue()));
    }

    public static V<? extends Integer> IINC(V<? extends Integer> a, int increment, FeatureExpr ctx) {
        return a.smap(ctx, aa -> aa.intValue() + increment);
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
                if (v instanceof Boolean) {
                    return !((Boolean) v);
                } else if (v instanceof Integer) {
                    return (Integer) v == 0;
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
                if (v instanceof Boolean) {
                    return ((Boolean) v);
                } else if (v instanceof Integer) {
                    return (Integer) v != 0;
                }
                else {
                    throw new RuntimeException("Unsupported type in whenEQ");
                }
            }
        });
    }

    public static FeatureExpr whenGT(V a) {
        return a.when(v -> {
            if (v == null)
                return false;
            else {
                if (v instanceof Boolean)
                    return (Boolean) v;
                else if (v instanceof Integer)
                    return (Integer) v > 0;
                else
                    throw new RuntimeException("Unsupported type in whenGT");
            }
        });
    }

    public static FeatureExpr whenGE(V<? extends Integer> a) {
        return a.when(v -> {
            if (v == null)
                return false;
            else
                return v.intValue() >= 0;
        });
    }

    public static FeatureExpr whenLT(V<? extends Integer> a) {
        return a.when(v -> {
            if (v == null)
                return false;
            else
                return v.intValue() < 0;
        });
    }

    public static FeatureExpr whenLE(V<? extends Integer> a) {
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

    public static FeatureExpr whenILE(V<? extends Integer> a, V<? extends Integer> b) {
        V<? extends Integer> sub = ISUB(a, b);
        return whenLE(sub);
    }

    public static FeatureExpr whenINE(V<? extends Integer> a, V<? extends Integer> b) {
        V<? extends Integer> sub = ISUB(a, b);
        return whenNE(sub);
    }

    public static FeatureExpr whenIGT(V<? extends Integer> a, V<? extends Integer> b) {
        V<? extends Integer> sub = ISUB(a, b);
        return whenGT(sub);
    }

    public static V<? extends Integer> ISUB(V<? extends Integer> a, V<? extends Integer> b) {
        return a.flatMap(aa -> {
            if (aa == null)
                return V.one(null);
            else
                return b.map(bb -> {
                    if (bb == null)
                        return null;
                    else
                        return aa.intValue() - bb.intValue();
                });
        });
    }

    public static V<? extends Integer> IMUL(V<? extends Integer> a, V<? extends Integer> b, FeatureExpr ctx) {
        return a.sflatMap(ctx, (fe, aa) -> b.smap(fe, bb -> aa.intValue() * bb.intValue()));
    }

    public static V<? extends Integer> IDIV(V<? extends Integer> a, V<? extends Integer> b) {
        return a.flatMap(aa -> b.map(bb -> aa.intValue() / bb.intValue()));
    }

    public static V<? extends Integer> i2c(V<? extends Integer> a, FeatureExpr ctx) {
        return a.smap((v -> {
            int i = v.intValue();
            char c = (char)i;
            return (int) c;
        }), ctx);
    }

    public static FeatureExpr whenAEQ(V<?> a, V<?> b) {
        V<? extends Boolean> compare = a.flatMap(aa -> {
            return b.map(bb -> {
                return aa == bb;
            });
        });
        return compare.when(c -> c);
    }

    public static FeatureExpr whenANE(V<?> a, V<?> b) {
        V<? extends Boolean> compare = a.flatMap(aa -> {
            return b.map(bb -> {
                return aa != bb;
            });
        });
        return compare.when(c -> c);
    }

    public static V<? extends Integer> iushr(V<? extends Integer> value1, V<? extends Integer> value2, FeatureExpr ctx) {
        return value1.sflatMap(ctx, (fe, v1) -> value2.smap(fe, v2 -> v1 >>> v2));
    }

    public static V<? extends Integer> irem(V<? extends Integer> value1, V<? extends Integer> value2) {
        return value1.flatMap(v1 -> value2.map(v2 -> v1 % v2));
    }

    public static V<? extends Integer> ior(V<? extends Integer> value1, V<? extends Integer> value2) {
        return value1.flatMap(v1 -> value2.map(v2 -> v1 | v2));
    }
}
