package edu.cmu.cs.vbc.model.lang;

import de.fosd.typechef.featureexpr.FeatureExpr;
import edu.cmu.cs.varex.V;

/**
 * @author chupanw
 */
public class VStringBuilder {

    public static V append(java.lang.StringBuilder obj, V v, FeatureExpr ctx) {
        return v.smap(ctx, (x -> {
            java.lang.StringBuilder cloned = new java.lang.StringBuilder(obj.toString());
            return cloned.append(x);
        }));
    }

    public static V toString(java.lang.StringBuilder obj, FeatureExpr ctx) {
        return V.one(obj.toString());
    }
}
