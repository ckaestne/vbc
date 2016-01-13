package edu.cmu.cs.vbc;

import edu.cmu.cs.varex.V;

/**
 * Created by ckaestne on 12/26/2015.
 */
public class VBCLib {

    public static V<? extends Integer> iadd(V<? extends Integer> a, V<? extends Integer> b) {
        return a.flatMap(aa -> b.map(bb -> aa + bb));
    }
}
