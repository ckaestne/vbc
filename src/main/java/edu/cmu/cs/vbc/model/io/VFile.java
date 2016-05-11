package edu.cmu.cs.vbc.model.io;

import de.fosd.typechef.featureexpr.FeatureExpr;
import edu.cmu.cs.varex.V;
import edu.cmu.cs.varex.annotation.Immutable;
import edu.cmu.cs.vbc.model.lang.VBoolean;
import edu.cmu.cs.vbc.model.lang.VString;
import scala.collection.immutable.HashMap;

import java.io.File;

/**
 * Standard implementation of File is not immutable. For simplicity, we assume that
 * it is immutable.
 *
 * @author chupanw
 */
@Immutable
public class VFile {

    public File actual;

    public VFile(String n) {
        actual = new File(n);
    }

    public VBoolean exists(FeatureExpr ctx) {
        return new VBoolean(actual.exists());
    }

    public VString getAbsolutePath(FeatureExpr ctx) {
        return new VString(actual.getAbsolutePath());
    }

    //////////////////////////////////////////////////
    // V methods
    //////////////////////////////////////////////////

    public static V<? extends VFile> Vinit$Ljava_lang_String$V(V<? extends VString> v, FeatureExpr ctx) {
        return v.smap(ctx, x -> {
            return new VFile(x.toString());
        });
    }

}
