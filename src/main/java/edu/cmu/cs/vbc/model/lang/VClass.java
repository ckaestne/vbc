package edu.cmu.cs.vbc.model.lang;

import de.fosd.typechef.featureexpr.FeatureExpr;
import edu.cmu.cs.varex.V;
import edu.cmu.cs.varex.annotation.Immutable;
import edu.cmu.cs.vbc.model.net.VURL;


/**
 * @author chupanw
 */
@Immutable
public class VClass {

    public Class actual;

    public VClass(Class c) {
        actual = c;
    }

    //////////////////////////////////////////////////
    // V methods
    //////////////////////////////////////////////////

    public V<? extends VURL> getResource$Ljava_lang_String$Ljava_net_URL(V<? extends VString> vs, FeatureExpr ctx) {
        return vs.map(s -> new VURL(actual.getResource(s.toString())));
    }
}
