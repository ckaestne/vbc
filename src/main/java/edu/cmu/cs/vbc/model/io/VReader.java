package edu.cmu.cs.vbc.model.io;

import de.fosd.typechef.featureexpr.FeatureExpr;
import edu.cmu.cs.varex.annotation.Immutable;
import edu.cmu.cs.vbc.model.lang.VInteger;

import java.io.IOException;
import java.io.Reader;

/**
 * @author chupanw
 */
@Immutable
public abstract class VReader {

    abstract public VInteger read(FeatureExpr ctx) throws IOException;

    abstract public void close(FeatureExpr ctx) throws IOException;
}
