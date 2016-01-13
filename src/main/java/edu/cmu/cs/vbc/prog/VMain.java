package edu.cmu.cs.vbc.prog;

import edu.cmu.cs.varex.V;

/**
 * Created by ckaestne on 12/25/2015.
 */
public class VMain {
    public static void main(String[] args) {
        new VMain().run();
    }

    public VMain() {
        System.out.println(this.getClass().getClassLoader().toString());

    }

    public void run() {
        V<? extends VI> a = foo();
        V<? extends VI> b = bar();
        V<? extends VI> c = a.flatMap(v -> v.add(b));
        VI.print(c);
    }

    private V<? extends VI> bar() {

        return V.one(new VI(V.one(3)));
    }

    private V<? extends VI> foo() {
        return V.one(new VI(V.one(5)));
    }

}


class VI {
    final V<Integer> i;

    VI(V<Integer> i) {
        System.out.println("creating I." + i + " " + this.getClass().getClassLoader().toString());
        this.i = i;
    }

    V<? extends VI> add(V<? extends VI> that) {
        return that.<VI>flatMap(t -> this.i.<VI>flatMap(ii -> t.i.<VI>map(ti -> new VI(V.one(ii + ti)))));
    }

    static void print(V<? extends VI> that) {
        that.foreach(t -> System.out.println(t.i));
    }
}