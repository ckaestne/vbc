package edu.cmu.cs.vbc.model.io;

import de.fosd.typechef.featureexpr.FeatureExpr;
import edu.cmu.cs.varex.V;
import edu.cmu.cs.varex.annotation.Immutable;
import edu.cmu.cs.vbc.model.lang.VInteger;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

/**
 * @author chupanw
 */
@Immutable
public class VFileReader extends VReader {

    public FileReader actual;

    public VFileReader(VFile f) {
        try {
            actual = new FileReader(f.actual);
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        }
    }

    @Override
    public VInteger read(FeatureExpr ctx) throws IOException {
        return new VInteger(actual.read());
    }

    @Override
    public void close(FeatureExpr ctx) throws IOException {
        actual.close();
    }

    //////////////////////////////////////////////////
    // V methods
    //////////////////////////////////////////////////
    public static V<? extends VFileReader> Vinit$Ljava_io_File$V(V<? extends VFile> vf, FeatureExpr ctx) {
        return vf.smap(ctx, x -> new VFileReader(x));
    }

}
