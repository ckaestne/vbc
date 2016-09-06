package edu.cmu.cs.vbc.instr;

import java.util.LinkedList;
import java.util.List;
import java.util.function.Consumer;
import java.util.stream.Collectors;

/**
 * Examples of how INVOKEDYNAMIC is used in bytecode.
 * <p>
 * Seems that Java compiler mostly uses INVOKEDYNAMIC to create function objects.
 *
 * @author chupanw
 */
public class Example_INVOKEDYNAMIC {

    public static void main(String[] args) {
        new Example_INVOKEDYNAMIC().simpleLambda();
        new Example_INVOKEDYNAMIC().lambdaWithOneArgument();
        new Example_INVOKEDYNAMIC().map();
    }

    /**
     * Java 8 generates object that conforms to Runnable interface at runtime.
     */
    public void simpleLambda() {
        Runnable r = () -> System.out.println("Hello");
        r.run();
    }

    /**
     * Similarly, Java 8 generates object that conforms to Consumer interface at runtime and invoke the accept method.
     * Interface call (i.e. accept() in this case) is delegated to a MethodHandle.
     */
    public void lambdaWithOneArgument() {
        Consumer<Integer> fun = (Integer x) -> System.out.println(x);
        fun.accept(2);
    }

    /**
     * Although this example looks more complicated, internally this is similar to previous two. Java compiler use
     * INVOKEDYNAMIC to generate a function object that conforms to Function interface, and pass it to the map method
     * of Stream class.
     */
    public void map() {
        List<Integer> list = new LinkedList<>();
        list.add(1);
        list.add(2);
        List<Integer> transformed = list.stream().map(i -> i * 2).collect(Collectors.toList());
        System.out.println(transformed);
    }
}
