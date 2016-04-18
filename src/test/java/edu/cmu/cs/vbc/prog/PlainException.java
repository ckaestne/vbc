package edu.cmu.cs.vbc.prog;

/**
 * Created by ckaestne on 4/5/16.
 */
public class PlainException {

    public static void main(String[] args) {
        foo0();
        foo1();
        foo3();
        foo2();
        foo();
//        bar();


    }

    private static void foo0() {
        int i = 0;
        int j = 5;
        try {
            int x = j / i;
        } catch (ArithmeticException e) {
        }
    }

    private static void foo1() {
        int i = 0;
        int j = 5;
        System.out.println("start");
        try {
            int x = j / i;
        } catch (ArithmeticException e) {
        }
        System.out.println("done");

    }

    private static void foo2() {
        int i = 0;
        int j = 5;
        try {
            int x = j / i;
        } catch (ArithmeticException e) {
            System.out.println("exception");
        }
    }

    private static void foo3() {
        int i = 0;
        int j = 5;
        try {
            int x = j / i;
            System.out.println("done");
        } catch (ArithmeticException e) {
            System.out.println("exception");
        }
    }

    private static void foo() {
        int i = 0;
        int j = 5;
        try {
            System.out.println(i + j);
            int x = j / i;
            System.out.println(x);
        } catch (ArithmeticException e) {
            System.out.println("exception");
        }
        System.out.println("done");

    }

    private static void bar() {
        int i = 0;
        int j = 5;
        try {
            System.out.println(i + j);
        } finally {
            System.out.println("finally1");
        }

        try {
            int x = j / i;
            System.out.println(x);
        } finally {
            System.out.println("finally2");
        }

    }

}
