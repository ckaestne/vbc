package edu.cmu.cs.varex;

/**
 * Created by ckaestne on 1/16/2016.
 */
public class VOps {

  public static V<? extends Integer> IADD(V<? extends Integer> a, V<? extends Integer> b) {
    return a.flatMap(aa -> b.map(bb -> aa + bb));
  }
}
