package edu.cmu.cs.vbc.model.lang;

import de.fosd.typechef.featureexpr.FeatureExpr;
import edu.cmu.cs.varex.V;

/**
 * @author chupanw
 */
public class StringBuilder {

    public static V append(java.lang.StringBuilder obj, V v, FeatureExpr fe) {
        java.lang.StringBuilder ret = obj.append(v.vmap(fe, (ctx, x) -> x));
        return V.one(ret);
    }

    public static V toString(java.lang.StringBuilder obj, FeatureExpr fe) {
        return V.one(obj.toString());
    }
}
