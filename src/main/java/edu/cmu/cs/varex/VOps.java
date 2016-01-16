package edu.cmu.cs.varex;

/**
 * Created by ckaestne on 1/16/2016.
 */
public class VOps {

  public static V<? extends Integer> IADD(V<? extends Integer> a, V<? extends Integer> b) {
    return a.flatMap(aa -> b.map(bb -> aa + bb));
  }

  public static V<? extends Integer> IINC(V<? extends Integer> a, int increment) {
    return a.map(aa -> aa + increment);
  }
}
