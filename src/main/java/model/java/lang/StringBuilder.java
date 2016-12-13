package model.java.lang;

import de.fosd.typechef.featureexpr.FeatureExpr;
import edu.cmu.cs.varex.One;
import edu.cmu.cs.varex.V;

/**
 * @author chupanw
 */
public class StringBuilder {

    V<? extends java.lang.StringBuilder> vActual;

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
}
