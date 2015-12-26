package edu.cmu.cs.varex;

import de.fosd.typechef.featureexpr.FeatureExpr;
import de.fosd.typechef.featureexpr.FeatureExprFactory;

import java.util.function.*;

/**
 * Created by ckaestne on 11/27/2015.
 */
public class One<T> implements V<T> {
    final T value;

    public One(T v) {
        this.value = v;
    }

    @Override
    public String toString() {
        return "Value(" + value + ")";
    }

    @Override
    public T getOne() {
        return value;
    }

    @Override
    public T getOne(FeatureExpr ctx) {
        return getOne();
    }

    @Override
    public <U> V<? extends U> map(Function<? super T, ? extends U> fun) {
        return new One(fun.apply(value));
    }

    @Override
    public <U> V<? extends U> vmap(FeatureExpr ctx, BiFunction<FeatureExpr, ? super T, ? extends U> fun) {
        return new One(fun.apply(ctx, value));
    }

    @Override
    public <U> V<? extends U> flatMap(Function<? super T, V<? extends U>> fun) {
        return fun.apply(value);
    }

    @Override
    public <U> V<? extends U> vflatMap(FeatureExpr ctx, BiFunction<FeatureExpr, ? super T, V<? extends U>> fun) {
        return fun.apply(ctx, value);
    }

    @Override
    public void foreach(Consumer<T> fun) {
        fun.accept(value);
    }

    @Override
    public void vforeach(FeatureExpr ctx, BiConsumer<FeatureExpr, T> fun) {
        fun.accept(ctx, value);
    }

    @Override
    public FeatureExpr when(Predicate<T> condition) {
        return condition.test(value) ? FeatureExprFactory.True() : FeatureExprFactory.False();
    }

    @Override
    public int hashCode() {
        return value.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof One) {
            if (((One) obj).value == null) return value == null;
            return ((One)obj).value.equals(value);
        }
        return super.equals(obj);
    }
}
