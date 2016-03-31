package edu.cmu.cs.varex;

import de.fosd.typechef.featureexpr.FeatureExpr;
import de.fosd.typechef.featureexpr.FeatureExprFactory;

import java.util.*;
import java.util.function.*;

/**
 * internal implementation of V
 */
class VImpl<T> implements V<T> {

    static <U> V<? extends U> choice(FeatureExpr condition, U a, U b) {
        Map<U, FeatureExpr> result = new HashMap<>(2);
        if (condition.isSatisfiable())
            put(result, a, condition);
        else return V.one(b);
        if (condition.not().isSatisfiable())
            put(result, b, condition.not());
        else return V.one(a);

        return createV(result);
    }

    static <U> V<? extends U> choice(FeatureExpr condition, V<? extends U> a, V<? extends U> b) {
        Map<U, FeatureExpr> result = new HashMap<>(2);
        if (condition.isSatisfiable())
            addVToMap(result, condition, a);
        else return b;
        if (condition.not().isSatisfiable())
            addVToMap(result, condition.not(), b);
        else return a;

        return createV(result);
    }

    private VImpl(Map<T, FeatureExpr> values) {
        this.values = values;
        assert checkInvariant() : "invariants violated";
    }

    //invariant: nonempty, all FeatureExpr together yield true
    private final Map<T, FeatureExpr> values;

    private boolean checkInvariant() {
        if (values.isEmpty()) return false;// : "empty V";
        if (values.size()<2) return false;// : "singleton VImpl?";
        FeatureExpr conditions = FeatureExprFactory.False();
        for (FeatureExpr cond : values.values()) {
            if (!conditions.and(cond).isContradiction()) return false;// : "condition overlaps with previous condition";
            conditions = conditions.or(cond);
        }
        if (!conditions.isTautology()) return false;// : "conditions together not a tautology";

        return true;
    }


    @Override
    public T getOne() {
        assert false : "getOne called on Choice: " + this;
        return values.keySet().iterator().next();
    }

    @Override
    public T getOne(FeatureExpr ctx) {
        T result = null;
        boolean foundResult = false;
        for (HashMap.Entry<T, FeatureExpr> e : values.entrySet())
            if (ctx.and(e.getValue()).isSatisfiable()) {
                assert !foundResult : "getOne("+ctx+") called on Choice with multiple matching values: " + this;
                result = e.getKey();
                foundResult=true;
            }

        return result;
    }


    @Override
    public <U> V<? extends U> map(Function<? super T, ? extends U> fun) {
        Map<U, FeatureExpr> result = new HashMap<>(values.size());
        for (HashMap.Entry<T, FeatureExpr> e : values.entrySet())
            put(result, fun.apply(e.getKey()), e.getValue());
        return createV(result);
    }

    @Override
    public <U> V<? extends U> vmap(FeatureExpr ctx, BiFunction<FeatureExpr, ? super T, ? extends U> fun) {
        Map<U, FeatureExpr> result = new HashMap<>(values.size());
        for (HashMap.Entry<T, FeatureExpr> e : values.entrySet())
            put(result, fun.apply(ctx.and(e.getValue()), e.getKey()), e.getValue());
        return createV(result);
    }

    @Override
    public <U> V<? extends U> flatMap(Function<? super T, V<? extends U>> fun) {
        Map<U, FeatureExpr> result = new HashMap<>(values.size());
        for (HashMap.Entry<T, FeatureExpr> e : values.entrySet()) {
            V<? extends U> u = null;
            if (e.getKey() != null)
                u = fun.apply(e.getKey());
            addVToMap(result, e.getValue(), u);
        }
        return createV(result);
    }


    @Override
    public <U> V<? extends U> vflatMap(BiFunction<FeatureExpr, ? super T, V<? extends U>> fun, FeatureExpr ctx) {
        Map<U, FeatureExpr> result = new HashMap<>(values.size());
        for (HashMap.Entry<T, FeatureExpr> e : values.entrySet()) {
            V<? extends U> u = null;
            if (e.getKey() != null)
                u = fun.apply(ctx.and(e.getValue()), e.getKey());
            addVToMap(result, e.getValue(), u);
        }
        return createV(result);
    }

    private static <U> void addVToMap(Map<U, FeatureExpr> result, FeatureExpr ctx,  V<? extends U> u) {
        // this could happen when fields are partially initialized
        if (u == null) {
            u = new One<>(null);
        }
        assert (u instanceof One) || (u instanceof VImpl) : "unexpected V value: " + u;
        if (u instanceof One)
            put(result, ((One<U>) u).value, ctx);
        else
            for (HashMap.Entry<U, FeatureExpr> ee : ((VImpl<U>) u).values.entrySet()) {
                FeatureExpr cond = ctx.and(ee.getValue());
                if (cond.isSatisfiable())
                    put(result, ee.getKey(), cond);
            }
    }

    private static <U> void put(Map<U, FeatureExpr> map, U value, FeatureExpr condition) {
        if (map.containsKey(value))
            condition = condition.or(map.get(value));
        map.put(value, condition);
    }

    private static <U> V<? extends U> createV(Map<U, FeatureExpr> map) {
        assert map.size() > 0 : "empty result";
        if (map.size() == 1) {
            assert map.values().iterator().next().isTautology() : "incomplete result " + map;
            return new One(map.keySet().iterator().next());
        }
        return new VImpl<>(map);
    }

    @Override
    public void foreach(Consumer<T> fun) {
        values.keySet().forEach(fun::accept);

    }


    @Override
    public void vforeach(FeatureExpr ctx, BiConsumer<FeatureExpr, T> fun) {
        for (HashMap.Entry<T, FeatureExpr> e : values.entrySet())
            fun.accept(ctx.and(e.getValue()), e.getKey());
    }

    @Override
    public FeatureExpr when(Predicate<T> condition) {
        FeatureExpr result = FeatureExprFactory.False();
        for (HashMap.Entry<T, FeatureExpr> e : values.entrySet())
            if (condition.test(e.getKey()))
                result = result.or(e.getValue());
        return result;
    }

    @Override
    public String toString() {
        StringBuffer out = new StringBuffer();
        List<String> entries = new ArrayList<>(values.size());
        for (HashMap.Entry<T, FeatureExpr> e : values.entrySet())
            entries.add(e.getKey() + "<-" + e.getValue().toTextExpr());
        Collections.sort(entries);
        out.append("CHOICE(");
        for (String e : entries) {
            out.append(e);
            out.append("; ");
        }
        out.delete(out.length() - 2, out.length());
        out.append(")");


        return out.toString();
    }

    @Override
    public int hashCode() {
        return this.values.hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof VImpl) {
            return ((VImpl)obj).values.equals(values);
        }
        return super.equals(obj);
    }
}


