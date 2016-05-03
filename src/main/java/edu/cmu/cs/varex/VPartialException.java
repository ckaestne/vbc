package edu.cmu.cs.varex;

import de.fosd.typechef.featureexpr.FeatureExpr;

import javax.annotation.Nonnull;
import java.util.function.*;

/**
 * see https://github.com/ckaestne/vbc/wiki/Exceptions
 */
public class VPartialException<T> implements V<Object> {


    /**
     * condition under which this exception is relevant
     */
    @Nonnull
    private final FeatureExpr cond;

    /**
     * result of the method, both normal results and exceptions (the latter under condition cond).
     * <p>
     * never null, but may be One(null) for all non-exception results
     */
    @Nonnull
    private final V<?> result;


    public VPartialException(@Nonnull FeatureExpr cond, @Nonnull V<?> result) {
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


    @Override
    public Object getOne() {
        throw new UnsupportedOperationException("operation not supported on VPartialException");
    }

    @Override
    public <U> V<? extends U> map(@Nonnull Function<? super Object, ? extends U> fun) {
        throw new UnsupportedOperationException("operation not supported on VPartialException");
    }

    @Override
    public <U> V<? extends U> map(@Nonnull BiFunction<FeatureExpr, ? super Object, ? extends U> fun) {
        throw new UnsupportedOperationException("operation not supported on VPartialException");
    }

    @Override
    public <U> V<? extends U> flatMap(@Nonnull Function<? super Object, V<? extends U>> fun) {
        throw new UnsupportedOperationException("operation not supported on VPartialException");
    }

    @Override
    public <U> V<? extends U> flatMap(@Nonnull BiFunction<FeatureExpr, ? super Object, V<? extends U>> fun) {
        throw new UnsupportedOperationException("operation not supported on VPartialException");
    }

    @Override
    public void foreach(@Nonnull Consumer<Object> fun) {
        throw new UnsupportedOperationException("operation not supported on VPartialException");
    }

    @Override
    public void foreach(@Nonnull BiConsumer<FeatureExpr, Object> fun) {
        throw new UnsupportedOperationException("operation not supported on VPartialException");
    }

    @Override
    public FeatureExpr when(@Nonnull Predicate<Object> condition) {
        throw new UnsupportedOperationException("operation not supported on VPartialException");
    }

    @Override
    public V<Object> select(@Nonnull FeatureExpr configSpace) {
        throw new UnsupportedOperationException("operation not supported on VPartialException");
    }

    @Override
    public V<Object> reduce(@Nonnull FeatureExpr reducedConfigSpace) {
        throw new UnsupportedOperationException("operation not supported on VPartialException");
    }

    @Override
    public FeatureExpr getConfigSpace() {
        throw new UnsupportedOperationException("operation not supported on VPartialException");
    }
}

