package edu.cmu.cs.varex;

import java.io.IOException;

/**
 * Created by ckaestne on 11/27/2015.
 */
@FunctionalInterface
public interface FunctionIO<A,B> {
    B apply(A a) throws IOException;
}
