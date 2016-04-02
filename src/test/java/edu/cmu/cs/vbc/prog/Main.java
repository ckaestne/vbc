package edu.cmu.cs.vbc.prog;

/**
 * Created by ckaestne on 12/25/2015.
 */
public class Main {
    public static void main(String[] args) {
        new Main().run();
    }

    public Main() {
        System.out.println(this.getClass().getClassLoader().toString());

    }

    public void run() {
        I a = foo();
        I b = bar();
        I c = a.add(b);
        I.print(c);
    }

    private I bar() {

        return new I(3);
    }

    private I foo() {
        return new I(5);
    }

}

class I {
    private final int i;

    I(int i) {
        System.out.println("creating I." + i + " " + this.getClass().getClassLoader().toString());
        this.i = i;
    }

    I add(I that) {
        return new I(this.i + that.i);
    }

    static void print(I that) {
        System.out.println(that.i);
    }
}
