package edu.cmu.cs.varex;

/**
 * Created by ckaestne on 11/27/2015.
 */
@FunctionalInterface
public interface Function5<A,B,C,D,R> {
    R apply(A a, B b, C c, D d);
}
