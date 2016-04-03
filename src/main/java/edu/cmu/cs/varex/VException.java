package edu.cmu.cs.varex;

import de.fosd.typechef.featureexpr.FeatureExpr;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * see https://github.com/ckaestne/vbc/wiki/Exceptions
 */
public class VException extends RuntimeException {

    /**
     * condition under which this exception is relevant
     * (should be redundant to `e`'s configuration space)
     */
    @Nonnull
    final FeatureExpr cond;

    /**
     * exceptions thrown
     * <p>
     * config spaces of this `V` should be equivalent to `cond`
     */
    @Nonnull
    final V<? extends Throwable> e;

    /**
     * result of the method without exception.
     * <p>
     * null for methods with `void` return type, always nonnull otherwise
     */
    @Nullable
    final V<?> result;


    public VException(@Nonnull FeatureExpr cond, @Nonnull V<? extends Throwable> e, @Nullable V<?> result) {
        assert cond != null;
        assert e != null;
        assert e.getConfigSpace().equivalentTo(cond);

        this.cond = cond;
        this.e = e;
        this.result = result;
    }
}

