package edu.cmu.cs.vbc;

import de.fosd.typechef.featureexpr.FeatureExpr;
import de.fosd.typechef.featureexpr.FeatureExprFactory;
import edu.cmu.cs.varex.V;

/**
 * Created by ckaestne on 1/16/2016.
 */
public class Tmp {
  void test() {
    System.out.println(V.one(18));
    FeatureExpr a = FeatureExprFactory.True();
    FeatureExpr b = FeatureExprFactory.createDefinedExternal("B");
    FeatureExpr c = a.and(b);
    System.out.println(c);
  }
}
