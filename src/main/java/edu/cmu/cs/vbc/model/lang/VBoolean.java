package edu.cmu.cs.vbc.model.lang;

import edu.cmu.cs.varex.annotation.Immutable;

/**
 * @author chupanw
 */
@Immutable
public class VBoolean {
    public Boolean actual;

    public VBoolean(Boolean b) {
        actual = b;
    }

    public boolean booleanValue() {
        return actual;
    }

    public static VBoolean valueOf(boolean b) {
        return new VBoolean(b);
    }
}
