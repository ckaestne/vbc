package edu.cmu.cs.varex;

import de.fosd.typechef.featureexpr.FeatureExpr;

import javax.annotation.Nonnull;

/**
 * A choice of exceptions in the entire context of the method call
 * <p>
 * see https://github.com/ckaestne/vbc/wiki/Exceptions
 */
public class VException extends RuntimeException {

    /**
     * condition under which this exception is relevant
     * <p>
     * (should be equivalent to the method's context)
     */
    @Nonnull
    private final FeatureExpr cond;

    /**
     * result of the method, both normal results and exceptions (the latter under condition cond).
     * <p>
     * never null, but may be One(null) for all non-exception results
     */
    @Nonnull
    private final V<? extends Throwable> exceptions;


    public VException(@Nonnull FeatureExpr cond, @Nonnull V<? extends Throwable> exceptions) {
        assert cond != null;
        assert exceptions.getConfigSpace().equivalentTo(cond);
        exceptions.foreach(e -> {
            assert e instanceof Throwable;
        });

        this.cond = cond;
        this.exceptions = exceptions;
    }

    public V<? extends Throwable> getExceptions() {
        return this.exceptions.select(cond);
    }

    public FeatureExpr getExceptionCondition() {
        return cond;
    }
}

