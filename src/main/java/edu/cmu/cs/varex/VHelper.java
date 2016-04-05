package edu.cmu.cs.varex;

import de.fosd.typechef.featureexpr.FeatureExpr;
import de.fosd.typechef.featureexpr.FeatureExprFactory;

import java.util.HashMap;
import java.util.Map;

//
//import com.caucho.quercus.env.Value;
//import com.caucho.quercus.env.ValueOrVar;
//import com.caucho.quercus.env.Var;
//import de.fosd.typechef.featureexpr.FeatureExpr;
//import de.fosd.typechef.featureexpr.FeatureExprFactory;
//
//import javax.annotation.Nonnull;
//import java.lang.reflect.Array;
//import java.util.function.Function;
//import java.util.function.Predicate;
//import java.util.function.Supplier;
//
///**
// * Created by ckaestne on 11/27/2015.
// */
public class VHelper {

    public static <T> Map<FeatureExpr, T> explode(FeatureExpr ctx, V<T> v) {
        Map<FeatureExpr, T> result = new HashMap<>();
        v.sforeach(ctx, (c, a) -> result.put(c, a));
        return result;
    }

    public static FeatureExpr True() {
        return FeatureExprFactory.True();
    }

}

//
//  @Deprecated//use only as placeholder, use True instead if intentionally general context intended
//    public static FeatureExpr noCtx() {
////        System.err.println("missing context");
//        return FeatureExprFactory.True();
//    }
//
//    public static <A, B, C, R> V<? extends R> mapAll(V<? extends A> a, V<? extends B> b, V<? extends C> c, Function4<A, B, C, R> fun) {
//        return a.flatMap((aa) ->
//                b.flatMap((bb) ->
//                        c.<R>map((cc) -> fun.apply(aa, bb, cc))));
//    }
//
//    public static <A, B, R> V<? extends R> mapAll(V<? extends A> a, V<? extends B> b, Function3<A, B, R> fun) {
//        return a.flatMap((aa) ->
//                b.map((bb) -> fun.apply(aa, bb)));
//    }
//
//    public static <A, B, R> V<? extends R> flatMapAll(V<? extends A> a, V<? extends B> b, Function3<A, B, V<? extends R>> fun) {
//        return a.flatMap((aa) ->
//                b.flatMap((bb) -> fun.apply(aa, bb)));
//    }
//
//  public static <A, B, R> V<? extends R> vmapAll(FeatureExpr ctx, V<? extends A> a, V<? extends B> b, Function4<FeatureExpr, A, B, R> fun) {
//    return a.vflatMap(ctx, (c, aa) ->
//            b.vmap(c, (cc, bb) -> fun.apply(cc, aa, bb)));
//  }
//
//  public static <A, B, C, R> V<? extends R> vmapAll(FeatureExpr ctx, V<? extends A> a, V<? extends B> b, V<? extends C> c, Function5<FeatureExpr, A, B, C, R> fun) {
//    return a.<R>vflatMap(ctx, (cctx, aa) ->
//            b.<R>vflatMap(cctx, (ccctx, bb) ->
//                    c.<R>vmap(ccctx, (cccctx, cc) -> fun.apply(cccctx, aa, bb, cc))));
//  }
//
//  public static <A, B, R> V<? extends R> vflatMapAll(FeatureExpr ctx, V<? extends A> a, V<? extends B> b, Function4<FeatureExpr, A, B, V<? extends R>> fun) {
//    return a.vflatMap(ctx, (c, aa) ->
//            b.vflatMap(c, (cc, bb) -> fun.apply(cc, aa, bb)));
//  }
//
//
//  public static <T> V<T> toV(T v) {
//        return V.one(v);
//    }
//
//    public static <T> V<T> nonNull(V<T> t) {
//        assert(t!=null);
//        return t;
//    }
//
//  /**
//   * get a value from producer1, producer2, or producer3
//   * until predicate X is fulfilled (first solution is used)
//   *
//   * roughly
//   *   x=solution1
//   *   if (!predicate())
//   *     x=solution2
//   *   if (!predicate())
//   *     x=solution3
//   *   return x;
//   *
//   */
//
//  public static <T>  V<? extends T> vifTry(Supplier<V<? extends T>> solution1,
//                                                   Supplier<V<? extends T>> solution2,
//                                                   Predicate<T> predicate) {
//     V<? extends T> result = solution1.get();
//    return result.flatMap(r1 ->
//            predicate.test(r1) ?
//                    V.one(r1) :
//                    solution2.get()
//    );
//
//  }
//
//  public static <T> V<? extends T> vifTry3(Supplier<V<? extends T>> solution1,
//                                           Supplier<V<? extends T>> solution2,
//                                           Supplier<V<? extends T>> solution3,
//                                           Predicate<T> predicate) {
//    return vifTry(() -> vifTry(solution1, solution2, predicate), solution3, predicate);
//  }
//
//    public static Value[] arrayToOne(V<? extends Value>[] a) {
//        return mapArray(Value.class, a, x->x.getOne());
//    }
//    public static <T, U> U[] mapArray(Class<U> klass, T[] a, Function<T, U> f) {
//        U[] result = (U[]) Array.newInstance(klass, a.length);
//        for (int i=0; i<a.length;i++)
//            result[i]=f.apply(a[i]);
//        return result;
//
//
//    }
//    public static <T, U> V<? extends T> [] mapVArray(V<? extends U>[] a, Function<U, T> f) {
//        return mapArray(V.class, a, x->x.map(y->f.apply(y)));
//    }
//
//
//    public static <T> V<? extends T>[] toVArray(T[] a) {
//        return mapArray(V.class, a, x->V.one(x));
//    }
//
//  public static V<? extends Value> getValues( V<? extends ValueOrVar> v) {
//    return v.flatMap(a -> a == null ? V.one(null) : (a.isVar() ? a._var()._getValues() : V.one(a._value())));
//  }
//  @Deprecated //use when further investigation is required
//  public static V<? extends Value> _getValues( V<? extends ValueOrVar> v) {
//    return getValues(v);
//  }
//
//  @Deprecated//don't use permanently
//  public static Var[] valArrayToVar(Value[] args) {
//    return VHelper.mapArray(Var.class, args, a->Var.create(a));
//  }
//
//  public static <T> boolean vequal(V<? extends T> a, V<? extends T> b) {
//    return vmapAll(True(), a, b, (c, aa, bb) -> c.isContradiction() || aa.equals(bb)).when(x->x).isTautology();
//  }
//
//  public static void assertTrue(FeatureExpr ctx) {
//    if (!ctx.isTautology())
//      throw new UnimplementedVException("incomplete V transformation, cannot be called in context " + ctx);
//  }
//}
