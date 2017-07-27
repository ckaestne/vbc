package model.java.util;

import de.fosd.typechef.featureexpr.FeatureExpr;
import edu.cmu.cs.varex.V;

/**
 * @author chupanw
 */
public interface List extends Collection {
    V<?> add__Ljava_lang_Object__Z(V<?> elem, FeatureExpr ctx);

    V<?> size____I(FeatureExpr ctx);

    V<?> get__I__Ljava_lang_Object(V<? extends Integer> index, FeatureExpr ctx);

    V<?> sort__Lmodel_java_util_Comparator__V(V<Comparator> vComparator, FeatureExpr ctx);

    V<?> isEmpty____Z(FeatureExpr ctx);

    V<?> iterator____Ljava_util_Iterator(FeatureExpr ctx);

    V<?> clear____V(FeatureExpr ctx);

    V<?> contains__Ljava_lang_Object__Z(V<?> vO, FeatureExpr ctx);

    V<?> remove__Ljava_lang_Object__Z(V<?> vO, FeatureExpr ctx);
}