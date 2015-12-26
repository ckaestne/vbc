package edu.cmu.cs.varex;

import de.fosd.typechef.featureexpr.FeatureExpr;

/**
 * Created by ckaestne on 11/28/2015.
 */
public class OptImpl<T> implements Opt<T> {
    private final FeatureExpr condition;
    private final T value;

    public OptImpl(FeatureExpr condition, T value) {
        this.condition = condition;
        this.value = value;
    }

    @Override
    public T getValue() {
        return value;
    }

    @Override
    public FeatureExpr getCondition() {
        return condition;
    }
}
