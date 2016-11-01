package edu.cmu.cs.vbc.prog;

import edu.cmu.cs.varex.annotation.VConditional;

import java.util.ArrayList;

/**
 * @author chupanw
 */
public class ArrayListExample {

    @VConditional
    boolean A;
    private ArrayList<MyInteger> array;

    private ArrayListExample() {
        array = new ArrayList<>();
        if (A) {
            array.add(new MyInteger(1));
        } else {
            array.add(new MyInteger(2));
        }
    }

    private void testIterator() {
        if (A) {
            array.add(new MyInteger(3));
        }
        // stress test
        // disabled by default, otherwise takes too much time to run the test suite
//        if (A) {
//            for (int i = 0; i < 1000000; i++) {
//                array.add(new MyInteger(i));
//            }
//        }
        for (MyInteger i : array) {
                i.increment();
                i.increment();
        }
        for (MyInteger i : array) {
            System.out.println(i.toString());
        }
    }

    public static void main(String[] args) {
        ArrayListExample example = new ArrayListExample();
        System.out.println(example.array.get(0).toString());
        example.testIterator();
    }
}

/**
 * Wrapper for Integer.
 *
 * Add increment() method to check the side effect to array elements.
 */
class MyInteger {
    private Integer v;
    MyInteger(Integer v) {
        this.v = v;
    }
    @Override
    public String toString() {
        return "MyInteger{" +
                "v=" + v +
                '}';
    }
    void increment() {
        this.v += 1;
    }
}
