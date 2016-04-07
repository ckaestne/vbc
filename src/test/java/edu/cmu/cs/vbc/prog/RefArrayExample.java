package edu.cmu.cs.vbc.prog;

import edu.cmu.cs.varex.annotation.VConditional;
import java.lang.Integer;

/**
 * Adapted from JVM specification
 *
 * @author chupanw
 */
public class RefArrayExample {

    @VConditional
    boolean A;

    Integer arrayLength;

    public static void main(String[] args) {
        new RefArrayExample().createIntegerArray();
        new RefArrayExample().printIntegerArray();
        new RefArrayExample().createVIntegerArray();
        new RefArrayExample().potentialNullArrayRef();
        new RefArrayExample().arrayWithVIndex();
    }

    /**
     * Basic array creation, store and load
     *
     * Array reference could be non-V
     */
    public void createIntegerArray() {
        Integer integers[];
        int count = 10;
        integers = new Integer[count];
        integers[0] = 0;
        integers[1] = 1;
        System.out.println(integers[0]);
        System.out.println(integers[1]);
    }

    /**
     * Passing array as argument and print array element
     *
     * Array reference should be a V because we are passing it as an argument
     */
    public void printIntegerArray() {
        int count = 10;
        Integer[] integers = new Integer[count];
        integers[0] = 2;
        printArray0(integers);
    }

    /**
     * Conditional array length
     *
     * Array reference should be a V because of the conditional length
     */
    public void createVIntegerArray() {
        int count;
        if (A)
            count = 10;
        else
            count = 20;
        Integer[] integers = new Integer[count];

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
        Integer[] integers = new Integer[arrayLength];
        integers[0] = 5;
        printArray0(integers);
    }

    public void arrayWithVIndex() {
        int index = 0;
        if (A)
            index = 1;
        Integer[] integers = new Integer[10];
        integers[0] = 6;
        integers[1] = 7;
        System.out.println(integers[index]);
    }

    private void printArray0(Integer[] integers) {
        System.out.println(integers[0]);
    }
}
