package edu.cmu.cs.vbc.model.util;

import de.fosd.typechef.featureexpr.FeatureExpr;
import edu.cmu.cs.varex.V;
import edu.cmu.cs.vbc.model.lang.VBoolean;
import edu.cmu.cs.vbc.model.lang.VInteger;

import java.util.LinkedList;

/**
 * Variational LinkedList
 *
 * Implemented V[LinkedList], always split if operating in sub-context. For example,
 *
 * a = One(True: {1, 2})
 *
 * Adding "3" under ctx A results into:
 *
 * a = Choice(A, {1,2, 3}, {1, 2})
 *
 * Todo: for performance, maybe we should also consider shrinking
 *
 * @author chupanw
 */
public class VLinkedList {

    class IdentityLinkedList extends LinkedList {
        @Override
        public boolean equals(Object o) {
            return this == o;
        }
    }

    V<IdentityLinkedList> actual;

    public VLinkedList(FeatureExpr ctx) {
        actual = V.one(ctx, new IdentityLinkedList());
    }

    public void split(FeatureExpr ctx) {
        if (actual.select(ctx).equalValue(actual.select(ctx.not()))) {
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

    public void add(V<? extends VInteger> vi, V<? extends Object> vo, FeatureExpr ctx) {
        split(ctx);
        vi.sforeach(ctx, i -> {
            vo.sforeach(ctx, o -> {
                actual.sforeach(ctx, l -> {
                    l.add(i.actual, o);
                });
            });
        });
    }

    public void addFirst(V<? extends Object> ele, FeatureExpr ctx) {
        split(ctx);
        ele.sforeach(ctx, e -> {
            actual.sforeach(ctx, l -> {
                l.addFirst(e);
            });
        });
    }

    public V<? extends Object> removeFirst(FeatureExpr ctx) {
        split(ctx);
        return actual.smap(ctx, l -> {
            return l.removeFirst();
        });
    }

    public V<? extends VInteger> size(FeatureExpr ctx) {
        return actual.smap(ctx, x -> new VInteger(x.size()));
    }

    public V<? extends Object> get(V<? extends VInteger> vi, FeatureExpr ctx) {
        return vi.sflatMap(ctx, ii -> {
            return actual.smap(ctx, ll -> {
                return ll.get(ii.actual.intValue());
            });
        });
    }

    public V<? extends VInteger> indexOf(V<? extends Object> vo, FeatureExpr ctx) {
        return vo.sflatMap(ctx, o -> {
            return actual.smap(ctx, l -> {
                return new VInteger(l.indexOf(o));
            });
        });
    }

    public V<? extends Object> remove(V<? extends VInteger> vi, FeatureExpr ctx) {
        split(ctx);
        return vi.sflatMap(ctx, i -> {
            return actual.smap(ctx, l -> {
                return l.remove(i.actual.intValue());
            });
        });
    }
}

