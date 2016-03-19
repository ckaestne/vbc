package edu.cmu.cs.vbc.model;

import de.fosd.typechef.featureexpr.FeatureExpr;
import edu.cmu.cs.varex.One;
import edu.cmu.cs.varex.V;

/**
 * Temporary model class for java.lang.Integer
 *
 * @author chupanw
 */
public class VInteger {
    public Integer value;

    public VInteger(Integer value) {
        this.value = value;
    }

    public VInteger(Integer value, FeatureExpr fe) {
        this.value = value;
    }

    public VInteger(V<Integer> value) {
        this.value = ((Integer) ((One) value).getOne());
    }

    public V<? extends Integer> compareTo(V<? extends VInteger> another, FeatureExpr fe) {
        return another.vmap(fe, (ctx, x) -> value.compareTo(x.value));
    }

    public Integer compareTo(VInteger another) {
        return value.compareTo(another.value);
    }
}
