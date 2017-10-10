package model.java.lang;

import de.fosd.typechef.featureexpr.FeatureExpr;
import edu.cmu.cs.varex.ArrayOps;
import edu.cmu.cs.varex.One;
import edu.cmu.cs.varex.V;

/**
 * StringBuilder might be used in two scenarios.
 *
 * If it is used in lifted bytecode, great.
 *
 * If it is used in unlifted bytecode, we need to be careful because Strings from conflicting sub-contexts
 * might get appended into one StringBuilder and cause unexpected behaviors. This scenario could happen if
 * StringBuilder is used in classes that we don't lift.
 *
 * @author chupanw
 */
public class StringBuilder {

    V<? extends java.lang.StringBuilder> vActual;
    java.lang.StringBuilder actual;

    public StringBuilder(V<? extends String> vS, FeatureExpr ctx, String dummy) {
        vActual = vS.smap(ctx, s -> new java.lang.StringBuilder(s));
    }

    public StringBuilder(FeatureExpr ctx) {
        vActual = V.one(ctx, new java.lang.StringBuilder());
    }

    public V<?> append__Ljava_lang_String__Lmodel_java_lang_StringBuilder(V<? extends String> vS, FeatureExpr ctx) {
        vActual = vActual.sflatMap(ctx, (fe, sb) -> {
            if (vS instanceof One)
                return V.one(ctx, sb.append(vS.getOne()));
            else
                return vS.smap(fe, s -> new java.lang.StringBuilder(sb.toString()).append(s));
        });
        return V.one(ctx, this);
    }

    public V<?> append__Z__Lmodel_java_lang_StringBuilder(V<?> vI, FeatureExpr ctx) {
        vActual = vActual.sflatMap(ctx, (fe, sb) -> vI.smap(fe, i -> {
            java.lang.StringBuilder cloned = new java.lang.StringBuilder(sb.toString());
            if (i instanceof java.lang.Integer) {
                cloned.append((java.lang.Integer)i != 0);
            }
            else if (i instanceof java.lang.Boolean) {
                cloned.append((java.lang.Boolean) i);
            }
            return cloned;
        }));
        return V.one(ctx, this);
    }

    public V<?> append__I__Lmodel_java_lang_StringBuilder(V<? extends java.lang.Integer> vI, FeatureExpr ctx) {
        vActual = vActual.sflatMap(ctx, (fe, sb) -> {
            if (vI instanceof One)
                return V.one(ctx, sb.append(vI.getOne()));
            else
                return vI.smap(fe, i -> new java.lang.StringBuilder(sb.toString()).append(i));
        });
        return V.one(ctx, this);
    }

    public V<?> append__Ljava_lang_Object__Lmodel_java_lang_StringBuilder(V<?> vO, FeatureExpr ctx) {
        vActual = vActual.sflatMap(ctx, (fe, sb) -> {
            if (vO instanceof One)
                return V.one(ctx, sb.append(vO.getOne()));
            else
                return vO.smap(fe, o -> new java.lang.StringBuilder(sb.toString()).append(o));
        });
        return V.one(ctx, this);
    }

    public V<?> append__C__Lmodel_java_lang_StringBuilder(V<? extends java.lang.Integer> vI, FeatureExpr ctx) {
        vActual = vActual.sflatMap(ctx, (fe, sb) -> {
            if (vI instanceof One)
                return V.one(ctx, sb.append((char)vI.getOne().intValue()));
            else
                return vI.smap(fe, i -> new java.lang.StringBuilder(sb.toString()).append((char)i.intValue()));
        });
        return V.one(ctx, this);
    }

    public V<?> toString____Ljava_lang_String(FeatureExpr ctx) {
        return vActual.smap(ctx, sb -> sb.toString());
    }

    public V<?> append__Array_C_I_I__Lmodel_java_lang_StringBuilder(V<V<java.lang.Integer>[]> vCArray,
                                                                    V<java.lang.Integer> vOffset,
                                                                    V<java.lang.Integer> vLen,
                                                                    FeatureExpr ctx) {
        vActual = vOffset.sflatMap(ctx, (fe1, offset) -> vLen.sflatMap(fe1, (fe2, len) -> vCArray.sflatMap(fe2, (fe3, cArray) -> {
            V array = ArrayOps.expandCArray(cArray, fe3);
            return array.sflatMap(fe3, (fe4, a) -> vActual.smap((FeatureExpr)fe4, sb -> {
                return new java.lang.StringBuilder(sb.toString()).append((char[]) a, offset, len);
            }));
        })));
        return V.one(ctx, this);
    }

    //////////////////////////////////////////////////
    // non-V part
    //////////////////////////////////////////////////
    public StringBuilder() {
        actual = new java.lang.StringBuilder();
    }

    public StringBuilder append(String s) {
        actual.append(s);
        return this;    // not creating new instances, following JDK style
    }

    public StringBuilder append(int i) {
        actual.append(i);
        return this;    // not creating new instances, following JDK style
    }

    public String toString() {
        return actual.toString();
    }
}
