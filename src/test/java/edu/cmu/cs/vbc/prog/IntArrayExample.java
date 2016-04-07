package edu.cmu.cs.vbc.prog;

/**
 * This example comes from JVM specification
 *
 * @author chupanw
 */
public class IntArrayExample {

    public static void main(String[] args) {
        new IntArrayExample().createBuffer();
    }

    /**
     * Needs to handle NEWARRAY, IALOAD, IASTORE
     */
    public void createBuffer() {
        int buffer[];
        int bufze = 100;
        int value = 12;
        buffer = new int[bufze];
        buffer[10] = value;
        value = buffer[11];
        System.out.println(value);
    }
}
