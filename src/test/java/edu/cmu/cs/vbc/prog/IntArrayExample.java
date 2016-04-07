package edu.cmu.cs.vbc.prog;

import edu.cmu.cs.varex.annotation.VConditional;

/**
 * This example comes from JVM specification
 *
 * @author chupanw
 */
public class IntArrayExample {

    @VConditional
    private boolean A;

    private int arrayLength;

    public static void main(String[] args) {
        new IntArrayExample().createBuffer();
        new IntArrayExample().printIntArray();
        new IntArrayExample().createVIntegerArray();
        new IntArrayExample().potentialNullArrayRef();
        new IntArrayExample().arrayWithVIndex();
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
//        value = buffer[11];
        System.out.println(buffer[10]);
    }

    public void printIntArray() {
        int count = 10;
        int[] integers = new int[count];
        integers[0] = 2;
        printArray0(integers);
    }

    public void createVIntegerArray() {
        int count;
        if (A)
            count = 10;
        else
            count = 20;
        int[] integers = new int[count];

        int value;
        if (A)
            value = 3;
        else
            value = 4;

        integers[0] = value;
        System.out.println(integers[0]);
    }

    /**
     * Array ref could potentially be null
     */
    public void potentialNullArrayRef() {
        if (A)
            arrayLength = 10;   // if !A, arrayLength should be null
        else
            arrayLength = 20;
        int[] integers = new int[arrayLength];
        integers[0] = 5;
        printArray0(integers);
    }

    public void arrayWithVIndex() {
        int index = 0;
        if (A)
            index = 1;
        int[] integers = new int[10];
        integers[0] = 6;
        integers[1] = 7;
        System.out.println(integers[index]);
    }

    private void printArray0(int[] integers) {
        System.out.println(integers[0]);
    }
}
