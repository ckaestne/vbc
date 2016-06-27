package edu.cmu.cs.varex;

import de.fosd.typechef.featureexpr.FeatureExpr;

import javax.annotation.Nonnull;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

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

    public static V<? extends Throwable> getExceptions(V<VException> ex) {
        return ex.flatMap(e -> e.exceptions.select(e.cond));
    }

    public FeatureExpr getExceptionCondition() {
        return cond;
    }

    public static Iterator<ExceptionConditionPair> getExceptionIterator(V<VException> ex) {
        V<? extends Throwable> allexceptions = getExceptions(ex);
        Map<FeatureExpr, ? extends Throwable> allExceptions = VHelper.explode(ex.getConfigSpace(), allexceptions);
        List<ExceptionConditionPair> result = new ArrayList<>(allExceptions.size());
        for (Map.Entry<FeatureExpr, ? extends Throwable> e : allExceptions.entrySet())
            result.add(new ExceptionConditionPair(e.getKey(), e.getValue()));
        return result.iterator();
    }

    public static class ExceptionConditionPair {
        public final Throwable exception;
        public final FeatureExpr cond;

        private ExceptionConditionPair(@Nonnull FeatureExpr cond, @Nonnull Throwable exception) {
            this.cond = cond;
            this.exception = exception;
        }
    }

    public static void test(V<VException> ex) {
        Throwable x = getExceptionIterator(ex).next().exception;
        System.out.println(x);
    }
}

