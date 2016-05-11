package edu.cmu.cs.vbc.model.net;

import de.fosd.typechef.featureexpr.FeatureExpr;
import edu.cmu.cs.varex.annotation.Immutable;
import edu.cmu.cs.vbc.model.lang.VString;

import java.net.URL;

/**
 * @author chupanw
 */
@Immutable
public class VURL {
    public URL actual;

    public VURL(URL u) {
        actual = u;
    }

    public VString getFile(FeatureExpr ctx) {
        return new VString(actual.getFile());
    }
}
