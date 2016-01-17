package edu.cmu.cs.vbc;

import de.fosd.typechef.featureexpr.FeatureExpr;
import de.fosd.typechef.featureexpr.FeatureExprFactory;
import edu.cmu.cs.varex.V;

/**
 * Created by ckaestne on 1/16/2016.
 */
public class Tmp {
  void test() {
//    l1:
//    System.out.println("1");
//    l2:
//    System.out.println("2");
//    l3:
//    System.out.println("3");
//    l4:
//    System.out.println("4");
//    l5:
//    System.out.println("5");



    System.out.println(V.one(18));
    FeatureExpr a = FeatureExprFactory.True();
    FeatureExpr b = FeatureExprFactory.createDefinedExternal("B");
    FeatureExpr c = a.and(b);
    if (c.isSatisfiable())
      System.out.println(c);
  }
}
