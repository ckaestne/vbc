package edu.cmu.cs.vbc.model.lang;

/**
 * @author chupanw
 */
public class VCharacter {
    public Character actual;

    public VCharacter(char c) {
        actual = c;
    }

    public char charValue() {
        return actual;
    }
}
