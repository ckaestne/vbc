package edu.cmu.cs.vbc.model.lang;

import de.fosd.typechef.featureexpr.FeatureExpr;
import edu.cmu.cs.varex.V;
import edu.cmu.cs.varex.annotation.Immutable;

/**
 * @author chupanw
 */
@Immutable
public class VObject {

    public Object actual;

    public VClass getClass(FeatureExpr ctx) {
        return new VClass(super.getClass());
    }

    public V<? extends VBoolean> equals$Ljava_lang_Object$Z(V<? extends VObject> vo, FeatureExpr ctx) {
        //TODO
        return V.one(ctx, new VBoolean(true));
    }

}
