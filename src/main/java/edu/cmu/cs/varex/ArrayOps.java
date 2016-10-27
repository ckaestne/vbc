package edu.cmu.cs.varex;

import de.fosd.typechef.featureexpr.FeatureExpr;

import java.util.ArrayList;
import java.util.function.BiFunction;
import java.util.function.Function;

/**
 * Helper class for array handling while lifting bytecode.
 *
 * @author chupanw
 */
public class ArrayOps {

    //////////////////////////////////////////////////
    // int
    //////////////////////////////////////////////////

    public static V<Integer>[] initIArray(Integer length, FeatureExpr ctx) {
        V<?>[] array = new V<?>[length];
        ArrayList<V<Integer>> arrayList = new ArrayList<>();
        for (int i = 0; i < length; i++) {
            arrayList.add(i, V.one(ctx, 0));
        }
        return arrayList.toArray((V<Integer>[])array);
    }

    public static V<Integer>[] initIArray(int length, FeatureExpr ctx) {
        return initIArray(Integer.valueOf(length), ctx);
    }

    //////////////////////////////////////////////////
    // char
    //////////////////////////////////////////////////

    public static V<Integer>[] initCArray(Integer length, FeatureExpr ctx) {
        return initIArray(length, ctx);
    }

    public static V<Integer>[] initCArray(int length, FeatureExpr ctx) {
        return initCArray(Integer.valueOf(length), ctx);
    }

    /**
     * Expand V<Character>[] to V<char[]>
     */
    public static V<?> expandCArray(V<Integer>[] array, FeatureExpr ctx) {
        return expandCArrayElements(array, ctx, 0, new ArrayList<>());
    }

    /**
     * Helper function for {@link #expandCArray(V[], FeatureExpr)}
     */
    private static V<?> expandCArrayElements(V<Integer>[] array, FeatureExpr ctx, Integer index, ArrayList<Character> soFar) {
        return array[index].sflatMap(ctx, new BiFunction<FeatureExpr, Integer, V<?>>() {
            @Override
            public V<?> apply(FeatureExpr featureExpr, Integer t) {
                ArrayList<Character> newArray = new ArrayList<Character>(soFar);
                newArray.add((char)t.intValue());
                if (index == array.length - 1) {
                    char[] result = new char[array.length];
                    for (int i = 0; i < array.length; i++) {
                        result[i] = newArray.get(i);
                    }
                    return V.one(featureExpr, result);
                } else {
                    return expandCArrayElements(array, ctx, index + 1, newArray);
                }
            }
        });
    }

    /**
     * Transform V<Character[]> to V<Character>[]
     */
    public static V<?>[] compressCArray(V<char[]> arrays) {
        V<?> sizes = arrays.map(new Function<char[], Integer>() {
            @Override
            public Integer apply(char[] t) {
                return t.length;
            }
        });
        Integer size = (Integer) sizes.getOne(); // if results in a choice, exceptions will be thrown.
        ArrayList<V<?>> array = new ArrayList<>();
        for (int i = 0; i < size; i++) {
            array.add(compressCArrayElement(arrays, i));
        }
        V<?>[] result = new V<?>[size];
        array.toArray(result);
        return result;
    }

    /**
     * Helper function for {@link #compressCArray(V)}
     */
    private static V<?> compressCArrayElement(V<char[]> arrays, Integer index) {
        return arrays.map(new Function<char[], Integer>() {
            @Override
            public Integer apply(char[] ts) {
                return (int)ts[index];
            }
        });
    }

    //////////////////////////////////////////////////
    // Object
    //////////////////////////////////////////////////

    /**
     * Transform V<T>[] to V<T[]>
     */
    public static <T> V<?> expandArray(V<T>[] array, FeatureExpr ctx) {
        return expandArrayElements(array, ctx, 0, new ArrayList<T>());
    }

    /**
     * Helper function for {@link #expandArray(V[], FeatureExpr)}
     */
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

    /**
     * Transform V<T[]> to V<T>[]
     */
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

    /**
     * Helper function for {@link #compressArray(V)}
     */
    private static <T> V<?> compressArrayElement(V<T[]> arrays, Integer index) {
        return arrays.map(new Function<T[], Object>() {
            @Override
            public Object apply(T[] ts) {
                return ts[index];
            }
        });
    }


    public static void copyVArray(V<?>[] from, V<?>[] to) {
        assert from.length == to.length;
        for (int i = 0; i < from.length; i++) {
            to[i] = V.choice(from[i].getConfigSpace(), from[i], to[i]);
        }
    }

    //////////////////////////////////////////////////
    // Others
    //////////////////////////////////////////////////

    public static void aastore(V[] arrayref, int index, V newValue, FeatureExpr ctx) {
        V oldValue = arrayref[index];
        V choice = V.choice(ctx, newValue, oldValue);
        arrayref[index] = choice;
    }
}
