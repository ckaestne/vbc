package edu.cmu.cs.vbc.prog;

/**
 * @author chupanw
 */
public class MethodOverloadingExample {

    MethodOverloadingExample(Integer i) {
    }

    MethodOverloadingExample(int j) {
    }

    MethodOverloadingExample(String s) {
    }

    public void foo(Integer i) {
        System.out.println("foo(Integer)");
    }

    public void foo(String s) {
        System.out.println("foo(String)");
    }

    public void foo(int i) {
        System.out.println("foo(int)");
    }

    public void foo(String[] s) {
        System.out.println("foo(String[])");
    }

    public void foo(Integer[] i) {
        System.out.println("foo(Integer[])");
    }

    public void foo(int[] i) {
        System.out.println("foo(int[])");
    }

    public void foo(int[][] i) {
        System.out.println("foo(int[][])");
    }

    public static void bar(int[] i) {
        System.out.println("bar(int[])");
    }

    public static void bar(Integer[] i) {
        System.out.println("bar(Integer[])");
    }

    public static void bar(int i) {
        System.out.println("bar(int)");
    }

    public static void bar(Integer i) {
        System.out.println("bar(Integer)");
    }

    public static void main(String[] args) {
        MethodOverloadingExample example = new MethodOverloadingExample(0);
        // ensure that we are calling the right methods
        System.out.print("calling foo(Integer): ");
        example.foo(new Integer(0));
        System.out.print("calling foo(String): ");
        example.foo("0");
        System.out.print("calling foo(int): ");
        example.foo(0);
        System.out.print("calling foo(String[]): ");
        example.foo(new String[]{"h"});
        System.out.print("calling foo(Integer[]): ");
        example.foo(new Integer[]{1, 2, 3});
        System.out.print("calling foo(int[]): ");
        example.foo(new int[]{1, 2, 3});
        System.out.print("calling foo(int[][]): ");
        example.foo(new int[][]{new int[]{1}, new int[]{2}, new int[]{3}});
        System.out.print("calling bar(int[]): ");
        bar(new int[]{1, 2, 3});
        System.out.print("calling bar(Integer[]): ");
        bar(new Integer[]{1, 2, 3});
        System.out.print("calling bar(int): ");
        bar(1);
        System.out.print("calling bar(Integer): ");
        bar(new Integer(1));
        System.out.print("-- Please check the output to ensure it is calling the right methods --");
    }
}
