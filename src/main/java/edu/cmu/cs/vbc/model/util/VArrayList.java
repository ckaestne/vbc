package edu.cmu.cs.vbc.model.util;

import de.fosd.typechef.featureexpr.FeatureExpr;
import edu.cmu.cs.varex.One;
import edu.cmu.cs.varex.V;
import edu.cmu.cs.vbc.model.lang.VBoolean;
import edu.cmu.cs.vbc.model.lang.VInteger;
import edu.cmu.cs.vbc.model.lang.VObject;

import java.util.ArrayList;
import java.util.Arrays;

/**
 * Variational ArrayList
 *
 * @author chupanw
 */
public class VArrayList {

    V<ArrayList> actual;

    public VArrayList(FeatureExpr ctx) {
        actual = V.one(ctx, new ArrayList());
    }

    private void split(FeatureExpr ctx) {
        if (actual.select(ctx) == actual.select(ctx.not())) {
            // need to split
            V cloned = actual.smap(ctx.not(), x -> {
                return x.clone();
            });
            actual = V.choice(ctx, actual.select(ctx), cloned);
        }
    }

    public V<? extends VBoolean> add(V<? extends Object> ele, FeatureExpr ctx) {
        split(ctx);
        return ele.sflatMap(ctx, x -> {
            return actual.smap(ctx, y -> {
                return new VBoolean(y.add(x));
            });
        });
    }

    public V<? extends VInteger> size(FeatureExpr ctx) {
        return actual.smap(ctx, l -> new VInteger(l.size()));
    }

    public V<? extends Object> get(V<? extends VInteger> vi, FeatureExpr ctx) {
        return vi.sflatMap(ctx, i -> {
            return actual.smap(ctx, l -> {
                return l.get(i.actual);
            });
        });
    }

    public V<? extends Object> set(V<? extends VInteger> vi, V<? extends Object> vo, FeatureExpr ctx) {
        split(ctx);
        return vi.sflatMap(ctx, i -> {
            return vo.sflatMap(ctx, o -> {
                return actual.smap(ctx, l -> {
                    return l.set(i.actual, o);
                });
            });
        });
    }

}
