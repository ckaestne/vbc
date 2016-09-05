package edu.cmu.cs.vbc.instr;

/**
 * Explore how CHECKCAST is used in bytecode
 *
 * @author chupanw
 */
public class Example_CHECKCAST {
    public int[] checkPrimitiveArrayType() {
        int[] a = new int[10];
        return (int[]) a;
    }

    public Integer[] checkObjectArrayType() {
        Integer[] a = new Integer[10];
        return (Integer[]) a;
    }

    public Integer checkObjectType() {
        Object a = 10;
        return (Integer) a;
    }

    public int[][] checkMultiPrimitiveArray() {
        int[][] a = new int[10][20];
        return (int[][]) a;
    }

    public Integer[][] checkMultiObjectArray() {
        Integer[][] a = new Integer[10][20];
        return (Integer[][]) a;
    }
}
