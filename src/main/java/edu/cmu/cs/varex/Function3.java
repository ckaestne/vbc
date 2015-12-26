package edu.cmu.cs.varex;

/**
 * Created by ckaestne on 11/27/2015.
 */
@FunctionalInterface
public interface Function3<A,B,C> {
    C apply(A a, B b);
}
