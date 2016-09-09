package edu.cmu.cs.vbc.prog;

import edu.cmu.cs.varex.annotation.VConditional;

import java.util.Collections;
import java.util.Comparator;
import java.util.LinkedList;

/**
 * Test variational LinkedList
 *
 * @author chupanw
 */
public class LinkedListExample {
    @VConditional
    public static boolean A;

    private LinkedList<Integer> list;

    public LinkedListExample() {
        list = new LinkedList<>();
        if (A) {
            list.add(2);
            list.add(1);
        } else {
            list.add(5);
            list.add(4);
            list.add(3);
        }
    }

    public static void main(String[] args) {
        LinkedListExample test = new LinkedListExample();
        test.printSize();
        test.printElements();
        test.binarySearch();
        test.sort();
        test.printSize();
        test.printElements();
        test.binarySearch();
    }

    void printElements() {
        for (int i = 0; i < list.size(); i++) {
            System.out.println(list.get(i));
        }
    }

    void printSize() {
        System.out.println(list.size());
    }

    void sort() {
        Collections.sort(list, new Comparator() {
            @Override
            public int compare(Object o1, Object o2) {
                Integer i1 = (Integer) o1;
                Integer i2 = (Integer) o2;
                return i1 - i2;
            }
        });
    }

    void binarySearch() {
        int key;
        if (A)
            key = 2;
        else
            key = 4;
        System.out.println(Collections.binarySearch(list, key, new Comparator() {
            @Override
            public int compare(Object o1, Object o2) {
                Integer i1 = (Integer) o1;
                Integer i2 = (Integer) o2;
                return i1 - i2;
            }
        }));
    }

}
