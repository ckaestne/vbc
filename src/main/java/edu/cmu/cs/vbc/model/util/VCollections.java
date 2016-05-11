package edu.cmu.cs.vbc.model.util;

import de.fosd.typechef.featureexpr.FeatureExpr;
import edu.cmu.cs.varex.V;
import edu.cmu.cs.vbc.model.lang.VInteger;

import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;

/**
 * @author chupanw
 */
public class VCollections {

    public static V binarySearch(
            V<? extends VLinkedList> vl,    //todo: type erasure, ArrayList
            V<? extends Object> vo,
            V<? extends Comparator> vc,
            FeatureExpr ctx
    ) {
        return vo.sflatMap(ctx, o -> {
            return vc.sflatMap(ctx, c -> {
                return vl.sflatMap(ctx, l -> {
                    return l.actual.smap(ctx, ll -> {
                        return new VInteger(Collections.binarySearch(ll, o, c));
                    });
                });
            });
        });
    }

    public static void sort(
            V<? extends VLinkedList> vl,    //todo: type erasure, ArrayList
            V<? extends Comparator> vc,
            FeatureExpr ctx
    ) {
        vl.sforeach(ctx, l -> {
            l.split(ctx);
            l.actual.sforeach(ctx, ll -> {
                vc.sforeach(ctx, c -> {
                    assert ll instanceof LinkedList;
                    Collections.sort(ll, c);
                });
            });
        });
    }
}
