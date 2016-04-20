package edu.cmu.cs.vbc.model.lang;

import de.fosd.typechef.featureexpr.FeatureExpr;
import edu.cmu.cs.varex.V;

/**
 * @author chupanw
 */
public class VStringBuilder {

    public StringBuilder actual;

    public VStringBuilder(FeatureExpr ctx) {
        actual = new StringBuilder();
    }

    public VStringBuilder(StringBuilder sb) {
        actual = sb;
    }

    public VStringBuilder append(VString vs, FeatureExpr ctx) {
        return new VStringBuilder(actual.append(vs.toString()));
    }

    public VString toString(FeatureExpr ctx) {
        return new VString(actual.toString());
    }

    //////////////////////////////////////////////////
    // V methods
    //////////////////////////////////////////////////

    public V<? extends VStringBuilder> append$I$Ljava_lang_StringBuilder(V<? extends VInteger> vi, FeatureExpr ctx) {
        return vi.map(x -> {
            return new VStringBuilder(new StringBuilder(actual).append(x.intValue()));
        });
    }
}
