package edu.cmu.cs.varex;

import de.fosd.typechef.featureexpr.FeatureExpr;

import java.util.Map;

/**
 * Created by ckaestne on 11/29/2015.
 */
public interface VMap<K,T> extends Map<K, V<? extends T>> {

    void put(FeatureExpr ctx, K key, T value);
    void put(FeatureExpr ctx, K key, V<? extends T> value);

}
