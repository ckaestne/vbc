package edu.cmu.cs.varex;

import de.fosd.typechef.featureexpr.FeatureExpr;

import java.util.function.*;

/**
 * central abstraction for conditional/variational values
 *
 * @param <T>
 */

public interface V<T> {

    @Deprecated
    T getOne();

    @Deprecated
    T getOne(FeatureExpr ctx);


    <U> V<? extends U> map(Function<? super T, ? extends U> fun);

    <U> V<? extends U> vmap(FeatureExpr ctx, BiFunction<FeatureExpr, ? super T, ? extends U> fun);

    <U> V<? extends U> flatMap(Function<? super T, V<? extends U>> fun);

    <U> V<? extends U> vflatMap(BiFunction<FeatureExpr, ? super T, V<? extends U>> fun, FeatureExpr ctx);

    void foreach(Consumer<T> fun);

    void vforeach(FeatureExpr ctx, BiConsumer<FeatureExpr, T> fun);

    FeatureExpr when(Predicate<T> condition);

    static <U> V<U> one( U v) {
        return new One(v);
    }

    static <U> V<? extends U> choice(FeatureExpr condition,  U a,  U b) {
        if (condition.isContradiction())
            return one(b);
        else if (condition.isTautology())
            return one(a);
        else
            return VImpl.choice(condition, a, b);
    }

    static <U> V<? extends U> choice(FeatureExpr condition, Supplier<U> a, Supplier<U> b) {
        if (condition.isContradiction())
            return one(b.get());
        else if (condition.isTautology())
            return one(a.get());
        else
            return VImpl.choice(condition, a.get(), b.get());
    }

    static <U> V<? extends U> choice(FeatureExpr condition, V<? extends U> a, V<? extends U> b) {
        if (condition.isContradiction())
            return b;
        else if (condition.isTautology())
            return a;
        else
            return VImpl.choice(condition, a, b);
    }

}
