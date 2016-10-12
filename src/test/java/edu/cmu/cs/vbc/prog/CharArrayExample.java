package edu.cmu.cs.vbc.prog;

import edu.cmu.cs.varex.annotation.VConditional;

/**
 * Adapted from IntArrayExample
 *
 * @author chupanw
 */
public class CharArrayExample {

    @VConditional
    private boolean A;

    private int arrayLength;

    public static void main(String[] args) {
        new CharArrayExample().createBuffer();
        new CharArrayExample().printCharArray();
        new CharArrayExample().createVCharArray();
        new CharArrayExample().potentialNullArrayRef();
        new CharArrayExample().arrayWithVIndex();
        new CharArrayExample().getChars();
    }

    public void getChars() {
        char[] chars = new char[20];
        String str;
        if (A) {
            str = "Hello world";
        } else {
            str = "Hi world";
        }
        str.getChars(0, 3, chars, 0);
        System.out.println(chars[1]);
    }

    public void createBuffer() {
        char buffer[];
        int bufze = 100;
        char value = 'a';
        buffer = new char[bufze];
        buffer[10] = value;
//        value = buffer[11];
        System.out.println(buffer[10]);
    }

    public void printCharArray() {
        char count = 10;
        char[] chars = new char[count];
        chars[0] = 'b';
        printArray0(chars);
    }

    public void createVCharArray() {
        int count;
        if (A)
            count = 10;
        else
            count = 20;
        char[] chars = new char[count];

        char value;
        if (A)
            value = 'c';
        else
            value = 'd';

        chars[0] = value;
        System.out.println(chars[0]);
    }

    /**
     * Array ref could potentially be null
     */
    public void potentialNullArrayRef() {
        if (A)
            arrayLength = 10;   // if !A, arrayLength should be null
        else
            arrayLength = 20;
        char[] chars = new char[arrayLength];
        chars[0] = 'e';
        printArray0(chars);
    }

    public void arrayWithVIndex() {
        int index = 0;
        if (A)
            index = 1;
        char[] chars = new char[10];
        chars[0] = 'f';
        chars[1] = 'g';
        System.out.println(chars[index]);
    }

    private void printArray0(char[] chars) {
        System.out.println(chars[0]);
    }
}
