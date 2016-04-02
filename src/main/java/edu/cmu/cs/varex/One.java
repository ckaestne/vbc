package edu.cmu.cs.varex;

import de.fosd.typechef.featureexpr.FeatureExpr;
import de.fosd.typechef.featureexpr.FeatureExprFactory;

import java.util.function.*;

/**
 * Created by ckaestne on 11/27/2015.
 */
public class One<T> implements V<T> {
    final FeatureExpr configSpace;
    final T value;

    public One(FeatureExpr configSpace, T v) {
        this.configSpace = configSpace;
        this.value = v;
    }

    @Override
    public String toString() {
        String condition = "";
        if (!configSpace.isTautology())
            condition = configSpace.toString() + ":";
        return "One(" + condition + value + ")";
    }

    @Override
    public T getOne() {
        return value;
    }

    @Override
    public <U> V<? extends U> map(Function<? super T, ? extends U> fun) {
        return new One(configSpace, fun.apply(value));
    }

    @Override
    public <U> V<? extends U> map(BiFunction<FeatureExpr, ? super T, ? extends U> fun) {
        return new One(configSpace, fun.apply(configSpace, value));
    }

    @Override
    public <U> V<? extends U> flatMap(Function<? super T, V<? extends U>> fun) {
        return fun.apply(value).select(configSpace);
    }

    @Override
    public <U> V<? extends U> flatMap(BiFunction<FeatureExpr, ? super T, V<? extends U>> fun) {
        return fun.apply(configSpace, value).select(configSpace);
    }

    @Override
    public void foreach(Consumer<T> fun) {
        fun.accept(value);
    }

    @Override
    public void foreach(BiConsumer<FeatureExpr, T> fun) {
        fun.accept(configSpace, value);
    }

    @Override
    public FeatureExpr when(Predicate<T> condition) {
        return condition.test(value) ? FeatureExprFactory.True() : FeatureExprFactory.False();
    }

    @Override
    public V<T> select(FeatureExpr selectConfigSpace) {
        assert selectConfigSpace.implies(configSpace).isTautology() :
                "selecting under broader condition (" + selectConfigSpace + ") than the configuration space described by One (" + configSpace + ")";

        FeatureExpr newCondition = configSpace.and(selectConfigSpace);
        if (newCondition.isSatisfiable())
            return new One(newCondition, value);
        else return VEmpty.instance();
    }

    @Override
    public FeatureExpr getConfigSpace() {
        return configSpace;
    }

    @Override
    public int hashCode() {
        return value.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof One) {
            if (((One) obj).value == null) return value == null;
            return ((One)obj).value.equals(value) && ((One)obj).configSpace.equivalentTo(configSpace);
        }
        return super.equals(obj);
    }
}
