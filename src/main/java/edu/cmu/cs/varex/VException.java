package edu.cmu.cs.varex;

import de.fosd.typechef.featureexpr.FeatureExpr;

import javax.annotation.Nonnull;

/**
 * see https://github.com/ckaestne/vbc/wiki/Exceptions
 */
public class VException extends RuntimeException {

    /**
     * condition under which this exception is relevant
     */
    @Nonnull
    final FeatureExpr cond;

    /**
     * result of the method, both normal results and exceptions (the latter under condition cond).
     * <p>
     * never null, but may be One(null) for all non-exception results
     */
    @Nonnull
    final V<?> result;


    public VException(@Nonnull FeatureExpr cond, @Nonnull V<?> result) {
        assert cond != null;
        result.select(cond).foreach(e -> {
            assert e instanceof Throwable;
        });

        this.cond = cond;
        this.result = result;
    }

    public V<? extends Throwable> getExceptions() {
        return (V<? extends Throwable>) this.result.select(cond);
    }

    public V<?> getResults() {
        return this.result.reduce(cond.not());
    }

    public FeatureExpr getExceptionCondition() {
        return cond;
    }
}

