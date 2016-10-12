package edu.cmu.cs.varex;

import de.fosd.typechef.featureexpr.FeatureExpr;

import java.util.ArrayList;
import java.util.function.BiFunction;
import java.util.function.Function;

/**
 * Created by ckaestne on 1/16/2016.
 */
public class VOps {

    public static V<? extends Integer> IADD(V<? extends Integer> a, V<? extends Integer> b) {
        return a.flatMap(aa -> {
            if (aa == null)
                return V.one(null);
            else
                return b.map(bb -> {
                    if (bb == null)
                        return null;
                    else
                        return aa.intValue() + bb.intValue();
                });
        });
    }

    public static V<? extends Integer> IINC(V<? extends Integer> a, int increment) {
        return a.map(aa -> {
            if (aa == null)
                return null;
            else {
                return aa.intValue() + increment;
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

    public static FeatureExpr whenGT(V<? extends Integer> a) {
        return a.when(v -> v.intValue() > 0);
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

    public static V<? extends Integer> IMUL(V<? extends Integer> a, V<? extends Integer> b) {
        return a.flatMap(aa -> {
            if (aa == null)
                return V.one(null);
            else
                return b.map(bb -> {
                    if (bb == null)
                        return null;
                    else
                        return aa.intValue() * bb.intValue();
                });
        });
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

    public static <T> V<?> expandArray(V<T>[] array, FeatureExpr ctx) {
        return expandArrayElements(array, ctx, 0, new ArrayList<T>());
    }

    private static <T> V<?> expandArrayElements(V<T>[] array, FeatureExpr ctx, Integer index, ArrayList<T> soFar) {
        return array[index].sflatMap(ctx, new BiFunction<FeatureExpr, T, V<?>>() {
            @Override
            public V<?> apply(FeatureExpr featureExpr, T t) {
                ArrayList<T> newArray = new ArrayList<T>(soFar);
                newArray.add(t);
                if (index == array.length - 1) {
                    return V.one(featureExpr, newArray.toArray());
                } else {
                    return expandArrayElements(array, ctx, index + 1, newArray);
                }
            }
        });
    }

    public static <T> V<?>[] compressArray(V<T[]> arrays) {
        V<?> sizes = arrays.map(new Function<T[], Integer>() {
            @Override
            public Integer apply(T[] t) {
                return t.length;
            }
        });
        Integer size = (Integer) sizes.getOne(); // if results in a choice, exceptions will be thrown.
        ArrayList<V<?>> array = new ArrayList<>();
        for (int i = 0; i < size; i++) {
            array.add(compressArrayElement(arrays, i));
        }
        V<?>[] results = new V<?>[size];
        array.toArray(results);
        return results;
    }

    private static <T> V<?> compressArrayElement(V<T[]> arrays, Integer index) {
        return arrays.map(new Function<T[], Object>() {
            @Override
            public Object apply(T[] ts) {
                return ts[index];
            }
        });
    }
}
