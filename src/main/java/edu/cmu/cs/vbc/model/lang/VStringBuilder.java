package edu.cmu.cs.vbc.model.lang;

import de.fosd.typechef.featureexpr.FeatureExpr;
import edu.cmu.cs.varex.V;
import edu.cmu.cs.varex.annotation.Immutable;

/**
 * @author chupanw
 */
@Immutable
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
        return vi.smap(ctx, x -> {
//            if (x != null)
                return new VStringBuilder(new StringBuilder(actual).append(x.intValue()));
//            else
//                return new VStringBuilder(new StringBuilder(actual).append(null));
        });
    }

    public V<? extends VStringBuilder> append$Ljava_lang_String$Ljava_lang_StringBuilder(V<? extends VString> vs, FeatureExpr ctx) {
        return vs.smap(ctx, x -> {
            return new VStringBuilder(new StringBuilder(actual).append(x));
        });
    }

    /**
     * Internally Boolean could be Integer
     */
    public V<? extends VStringBuilder> append$Z$Ljava_lang_StringBuilder(V vb, FeatureExpr ctx) {
        return vb.smap(ctx, x -> {
            if (x instanceof VBoolean) {

                return new VStringBuilder(new StringBuilder(actual).append(((VBoolean)x).actual));
            }
            else if (x instanceof VInteger) {
                return new VStringBuilder(new StringBuilder(actual).append(((VInteger)x).actual != 0));
            }
            else {
                throw new RuntimeException("Unsupported type");
            }
        });
    }
}
