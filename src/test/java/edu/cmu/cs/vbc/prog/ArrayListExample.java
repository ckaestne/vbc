package edu.cmu.cs.vbc.prog;

import edu.cmu.cs.varex.annotation.VConditional;

import java.util.ArrayList;

/**
 * @author chupanw
 */
public class ArrayListExample {

    @VConditional
    boolean A;
    ArrayList<Integer> array;

    ArrayListExample() {
        array = new ArrayList<>();
        if (A) {
            array.add(1);
        } else {
            array.add(2);
        }
    }

    public static void main(String[] args) {
        ArrayListExample example = new ArrayListExample();
        System.out.println(example.array.get(0));
    }
}
