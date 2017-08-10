package model.java.util;

import de.fosd.typechef.featureexpr.FeatureExpr;
import edu.cmu.cs.varex.ArrayOps;
import edu.cmu.cs.varex.V;
import model.Contexts;

/**
 * Performance reasons.
 *
 * Also because we are lifting Comparator
 *
 * @author chupanw
 */
public class Arrays {
    public static Object[] copyOf(Object[] original, int newLength) {
        return java.util.Arrays.copyOf(original, newLength);
    }
    public static V<?> copyOf__Array_Ljava_lang_Object_I__Array_Ljava_lang_Object(V<V[]> original, V<Integer> newLength, FeatureExpr ctx) {
        return original.sflatMap(ctx, (fe, va) -> newLength.smap(fe, integer -> {
            V[] copied = java.util.Arrays.copyOf(va, integer);
            if (integer > va.length)
                java.util.Arrays.fill(copied, va.length, integer, V.one(fe, null)); return copied;
        }));
    }

    public static char[] copyOf(char[] original, int newLength) {
        return java.util.Arrays.copyOf(original, newLength);
    }
    public static V<?> copyOf__Array_C_I__Array_C(V<V[]> original, V<Integer> newLength, FeatureExpr ctx) {
        return original.sflatMap(ctx, (fe, va) -> newLength.smap(fe, integer -> {
                V[] copied = java.util.Arrays.copyOf(va, integer);
                if (integer > va.length)
                    java.util.Arrays.fill(copied, va.length, integer, V.one(fe, 0));
                return copied;
        }));
    }

    public static void fill(int[] a, int val) {
        java.util.Arrays.fill(a, val);
    }
    public static V<?> fill__Array_I_I__V(V<V[]> varray, V<Integer> vvalue, FeatureExpr ctx) {
        vvalue.sforeach(ctx, value -> varray.sforeach(ctx, array -> {
            for (int i = 0; i < array.length; i++) {
                array[i] = V.choice(ctx, V.one(ctx, value), array[i]);
            }
        }));
        return null;    // dummy return value
    }

    public static <T> void sort(T[] array, model.java.util.Comparator comparator) {
        java.util.Arrays.sort(array, comparator::compare);
    }
    public static <T> V<?> sort__Array_Ljava_lang_Object_Lmodel_java_util_Comparator__V(V<V<T>[]> vArray, V<Comparator> vComparator, FeatureExpr ctx) {
        vArray.sforeach(ctx, (FeatureExpr fe, V<T>[] array) -> {
            vComparator.sforeach(fe, (FeatureExpr fe2, Comparator comparator) -> {
                V<T[]> expanded = ArrayOps.expandArray(array, fe2);
                Contexts.model_java_util_Comparator_compare = fe2;
                expanded.sforeach(fe2, (T[] expandedArray) -> java.util.Arrays.sort(expandedArray, comparator::compare));
                V[] compressed = ArrayOps.compressArray(expanded);
                ArrayOps.copyVArray(compressed, array);
            });
        });
        return null;    // dummy return value
    }

    public static long[] copyOf(long[] original, int newLength) {
        return java.util.Arrays.copyOf(original, newLength);
    }

    public static void sort(int[] a) {
        java.util.Arrays.sort(a);
    }

    public static int binarySearch(int[] array, int key) {
        return java.util.Arrays.binarySearch(array, key);
    }
}
