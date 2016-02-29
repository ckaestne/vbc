package edu.cmu.cs.varex;

import de.fosd.typechef.featureexpr.FeatureExpr;
import de.fosd.typechef.featureexpr.FeatureExprFactory;

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

  public static FeatureExpr whenEQ(V<? extends Integer> a) {
    return a.when(v -> v == 0);
  }

  private static FeatureExpr lastFexpr = FeatureExprFactory.True();

  public static void updateCtx(FeatureExpr fexpr) {
    if (!fexpr.equals(lastFexpr)) {
      lastFexpr = fexpr;
      System.out.print("[" + fexpr + "]");
    }
  }

}
