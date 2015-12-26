//package edu.cmu.cs.varex;
//
//import de.fosd.typechef.featureexpr.FeatureExpr;
//
//import java.util.function.Consumer;
//import java.util.function.Function;
//
///**
// * Created by ckaestne on 11/27/2015.
// */
//public class Choice<T> implements V<T> {
//    private final FeatureExpr condition;
//    private final T a;
//    private final T b;
//
//    public Choice(FeatureExpr condition, T a, T b) {
//        this.condition = condition;
//        this.a = a;
//        this.b = b;
//    }
//
//    @Override
//    public String toString() {
//        return "CHOICE(" + condition.toTextExpr() + " ? " + a + " : " + b + ")";
//    }
//
//    @Override
//    public T getOne() {
//        System.err.println("calling getOne on a Choice");
//        return a;
//    }
//
//    @Override
//    public <U> V<U> map(Function<T, U> fun) {
//        return new Choice<>(condition, fun.apply(a), fun.apply(b));
//    }
//
//    @Override
//    public <U> V<U> flatMap(Function<T, V<U>> fun) {
//        return new Choice<>(condition, fun.apply(a).getOne(), fun.apply(b).getOne());
//    }
//
//    @Override
//    public void foreach(Consumer<T> fun) {
//        fun.accept(a);
//        fun.accept(b);
//    }
//}
