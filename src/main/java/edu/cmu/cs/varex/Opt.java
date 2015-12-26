package edu.cmu.cs.varex;

import de.fosd.typechef.featureexpr.FeatureExpr;
import de.fosd.typechef.featureexpr.FeatureExprFactory;

/**
 * central abstraction for conditional/variational values
 *
 * @param <T>
 */
public interface Opt<T> {
    T getValue();
    FeatureExpr getCondition();
    static <U> Opt<U> create(FeatureExpr expr, U v) {
        return new OptImpl(expr, v);
    }
    static <U> Opt<U> create(U v) {
        return new OptImpl(FeatureExprFactory.True(), v);
    }
}
