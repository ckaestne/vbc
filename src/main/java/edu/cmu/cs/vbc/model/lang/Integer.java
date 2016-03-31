package edu.cmu.cs.vbc.model.lang;

import de.fosd.typechef.featureexpr.FeatureExpr;
import edu.cmu.cs.varex.V;

/**
 * @author chupanw
 */
public class Integer {

    public static V compareTo(java.lang.Integer obj, V<java.lang.Integer> v, FeatureExpr fe) {
        return v.map(obj::compareTo);
    }

    public static V valueOf(V<java.lang.Integer> v, FeatureExpr fe) {
        return v;
    }
}
