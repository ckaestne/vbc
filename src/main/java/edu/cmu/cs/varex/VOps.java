package edu.cmu.cs.varex;

import de.fosd.typechef.featureexpr.FeatureExpr;
import org.apache.commons.beanutils.BeanUtilsBean;
import sun.reflect.generics.reflectiveObjects.ParameterizedTypeImpl;

import java.io.PrintStream;
import java.lang.reflect.*;

/**
 * Created by ckaestne on 1/16/2016.
 */
public class VOps {

    public static V<? extends Integer> IADD(V<? extends Integer> a, V<? extends Integer> b, FeatureExpr ctx) {
        return a.sflatMap(ctx, (fe, aa) -> b.smap(fe, bb -> aa.intValue() + bb.intValue()));
    }

    public static V<? extends Integer> IINC(V<? extends Integer> a, int increment, FeatureExpr ctx) {
        return a.smap(ctx, aa -> aa.intValue() + increment);
    }

    /**
     * Called by lifted bytecode, compare with 0
     *
     * @param a
     * @return
     */
    public static FeatureExpr whenEQ(V<?> a) {
        return a.when(v -> {
            if (v instanceof Boolean)
                return !(Boolean) v ;
            else if (v instanceof Integer)
                return (Integer) v == 0;
            else
                throw new RuntimeException("Unsupported whenEQ type");
        }, true);
    }

    /**
     * Called by lifted bytecode, compare with 0
     *
     * @param a
     * @return
     */
    public static FeatureExpr whenNE(V<?> a) {
        return a.when(v -> {
            if (v instanceof Boolean)
                return (Boolean) v;
            else if (v instanceof Integer)
                return (Integer) v != 0;
            else
                throw new RuntimeException("Unsupported whenNE type");
        }, true);
    }

    public static FeatureExpr whenGT(V<? extends Integer> a) {
        return a.when(v -> v > 0, true);
    }

    public static FeatureExpr whenGE(V<? extends Integer> a) {
        return a.when(v -> v >= 0, true);
    }

    public static FeatureExpr whenLT(V<? extends Integer> a) {
        return a.when(v -> v < 0, true);
    }

    public static FeatureExpr whenLE(V<? extends Integer> a) {
        return a.when(v -> v <= 0, true);
    }

    public static FeatureExpr whenNONNULL(V<? extends Object> a) {
        return a.when(v -> v != null, false);
    }

    public static FeatureExpr whenNULL(V<? extends Object> a) {
        return a.when(v -> v == null, false);
    }

    public static FeatureExpr whenIEQ(V<? extends Integer> a, V<? extends Integer> b) {
        V<? extends Integer> sub = compareInt(a, b);
        return whenEQ(sub);
    }

    public static FeatureExpr whenIGE(V<? extends Integer> a, V<? extends Integer> b) {
        V<? extends Integer> sub = compareInt(a, b);
        return whenGE(sub);
    }

    public static FeatureExpr whenILT(V<? extends Integer> a, V<? extends Integer> b) {
        V<? extends Integer> sub = compareInt(a, b);
        return whenLT(sub);
    }

    public static FeatureExpr whenILE(V<? extends Integer> a, V<? extends Integer> b) {
        V<? extends Integer> sub = compareInt(a, b);
        return whenLE(sub);
    }

    public static FeatureExpr whenINE(V<? extends Integer> a, V<? extends Integer> b) {
        V<? extends Integer> sub = compareInt(a, b);
        return whenNE(sub);
    }

    public static FeatureExpr whenIGT(V<? extends Integer> a, V<? extends Integer> b) {
        V<? extends Integer> sub = compareInt(a, b);
        return whenGT(sub);
    }

    private static V<? extends Integer> compareInt(V<? extends Integer> a, V<? extends Integer> b) {
        return a.flatMap(aa -> {
            if (aa == null)
                return V.one(null);
            else
                return b.map(bb -> {
                    if (bb == null)
                        return null;
                    else {
                        // avoid Integer overflow
                        if (aa.intValue() > 0 && bb.intValue() < 0)
                           return 1;
                        else if (aa.intValue() < 0 && bb.intValue() > 0)
                            return -1;
                        else
                            return aa.intValue() - bb.intValue();
                    }
                });
        });
    }

    public static V<? extends Integer> ISUB(V<? extends Integer> a, V<? extends Integer> b, FeatureExpr ctx) {
        return a.sflatMap(ctx, (fe, aa) -> b.smap(fe, bb -> aa.intValue() - bb.intValue()));
    }


    public static V<? extends Integer> IMUL(V<? extends Integer> a, V<? extends Integer> b, FeatureExpr ctx) {
        return a.sflatMap(ctx, (fe, aa) -> b.smap(fe, bb -> aa.intValue() * bb.intValue()));
    }

    public static V<? extends Integer> IDIV(V<? extends Integer> a, V<? extends Integer> b, FeatureExpr ctx) {
        return a.sflatMap(ctx, (fe, aa) -> b.smap(fe, bb -> aa.intValue() / bb.intValue()));
    }

    public static V<? extends Integer> i2c(V<? extends Integer> a, FeatureExpr ctx) {
        return a.smap((v -> {
            int i = v.intValue();
            char c = (char)i;
            return (int) c;
        }), ctx);
    }

    public static FeatureExpr whenAEQ(V<?> a, V<?> b) {
        V<? extends Boolean> compare = a.flatMap(aa -> {
            return b.map(bb -> {
                return aa == bb;
            });
        });
        return compare.when(c -> c, true);
    }

    public static FeatureExpr whenANE(V<?> a, V<?> b) {
        V<? extends Boolean> compare = a.flatMap(aa -> {
            return b.map(bb -> {
                return aa != bb;
            });
        });
        return compare.when(c -> c, true);
    }

    public static V<? extends Integer> iushr(V<? extends Integer> value1, V<? extends Integer> value2, FeatureExpr ctx) {
        return value1.sflatMap(ctx, (fe, v1) -> value2.smap(fe, v2 -> v1 >>> v2));
    }

    public static V<? extends Integer> irem(V<? extends Integer> value1, V<? extends Integer> value2) {
        return value1.flatMap(v1 -> value2.map(v2 -> v1 % v2));
    }

    public static V<? extends Integer> ior(V<? extends Integer> value1, V<? extends Integer> value2, FeatureExpr ctx) {
        return value1.sflatMap(ctx, (fe, v1) -> value2.smap(fe, v2 -> v1.intValue() | v2.intValue()));
    }

    public static V<? extends Integer> iand(V<? extends Integer> value1, V<? extends Integer> value2, FeatureExpr ctx) {
        return value1.sflatMap(ctx, (fe, v1) -> value2.smap(fe, v2 -> v1.intValue() & v2.intValue()));
    }

    public static V<? extends Integer> ixor(V<? extends Integer> value1, V<? extends Integer> value2, FeatureExpr ctx) {
        return value1.sflatMap(ctx, (fe, v1) -> value2.smap(fe, v2 -> v1 ^ v2));
    }

    public static V<? extends Long> ldiv(V<? extends Long> value1, V<? extends Long> value2, FeatureExpr ctx) {
        return value1.sflatMap(ctx, (fe, v1) -> value2.smap(fe, v2 -> v1.longValue() / v2.longValue()));
    }

    public static V<? extends Integer> l2i(V<? extends Long> value1, FeatureExpr ctx) {
        return value1.smap(ctx, x -> (int) x.longValue());
    }

    public static V<? extends Integer> i2b(V<? extends Integer> value1, FeatureExpr ctx) {
        return value1.smap(ctx, x -> (int) (byte) x.intValue());
    }

    public static V<? extends Integer> i2s(V<? extends Integer> value1, FeatureExpr ctx) {
        return value1.smap(ctx, x -> (int) (short) x.intValue());
    }

    public static V<? extends Long> i2l(V<? extends Integer> value1, FeatureExpr ctx) {
        return value1.smap(ctx, x -> (long) x.intValue());
    }

    public static V<? extends Long> ladd(V<? extends Long> value1, V<? extends Long> value2, FeatureExpr ctx) {
        return value1.sflatMap(ctx, (fe, v1) -> value2.smap(fe, v2 -> v1.longValue() + v2.longValue()));
    }

    public static V<? extends Long> land(V<? extends Long> value1, V<? extends Long> value2, FeatureExpr ctx) {
        return value1.sflatMap(ctx, (fe, v1) -> value2.smap(fe, v2 -> v1.longValue() & v2.longValue()));
    }

    public static V<? extends Long> lushr(V<? extends Long> value1, V<? extends Integer> value2, FeatureExpr ctx) {
        return value1.sflatMap(ctx, (fe, v1) -> value2.smap(fe, v2 -> v1.longValue() >>> v2.intValue()));
    }

    public static V<? extends Long> lsub(V<? extends Long> value1, V<? extends Long> value2, FeatureExpr ctx) {
        return value1.sflatMap(ctx, (fe, v1) -> value2.smap(fe, v2 -> v1.longValue() - v2.longValue()));
    }

    public static V<? extends Integer> ineg(V<? extends Integer> value1, FeatureExpr ctx) {
        return value1.smap(ctx, v -> -v.intValue());
    }

    public static V<? extends Double> dmul(V<? extends Double> value1, V<? extends Double> value2, FeatureExpr ctx) {
        return value1.sflatMap(ctx, (fe, v1) -> value2.smap(fe, v2 -> v1.doubleValue() * v2.doubleValue()));
    }

    public static V<? extends Integer> dcmpl(V<? extends Double> value1, V<? extends Double> value2, FeatureExpr ctx) {
        return value1.sflatMap(ctx, (fe, v1) -> value2.smap(fe, v2 -> {
            if (v1.isNaN() || v2.isNaN()) return -1;
            else if (v1.doubleValue() > v2.doubleValue()) return 1;
            else if (v1.doubleValue() == v2.doubleValue()) return 0;
            else return -1;
        }));
    }

    public static V<? extends Long> lmul(V<? extends Long> value1, V<? extends Long> value2, FeatureExpr ctx) {
        return value1.sflatMap(ctx, (fe, v1) -> value2.smap(fe, v2 -> v1.longValue() * v2.longValue()));
    }

    public static V<? extends Long> lor(V<? extends Long> value1, V<? extends Long> value2, FeatureExpr ctx) {
        return value1.sflatMap(ctx, (fe, v1) -> value2.smap(fe, v2 -> v1.longValue() | v2.longValue()));
    }

    public static V<? extends Long> lshl(V<? extends Long> value1, V<? extends Integer> value2, FeatureExpr ctx) {
        return value1.sflatMap(ctx, (fe, v1) -> value2.smap(fe, v2 -> v1.longValue() << v2.intValue()));
    }

    public static V<? extends Long> lxor(V<? extends Long> value1, V<? extends Long> value2, FeatureExpr ctx) {
        return value1.sflatMap(ctx, (fe, v1) -> value2.smap(fe, v2 -> v1.longValue() ^ v2.longValue()));
    }

    public static V<? extends Double> l2d(V<? extends Long> value1, FeatureExpr ctx) {
        return value1.smap(ctx, l -> (double) l.longValue());
    }

    public static V<? extends Double> i2d(V<? extends Integer> value1, FeatureExpr ctx) {
        return value1.smap(ctx, i -> (double) i.intValue());
    }

    public static V<? extends Float> d2f(V<? extends Double> value1, FeatureExpr ctx) {
        return value1.smap(ctx, d -> (float) d.doubleValue());
    }

    public static V<? extends Double> f2d(V<? extends Float> value1, FeatureExpr ctx) {
        return value1.smap(ctx, f -> (double) f.floatValue());
    }

    //////////////////////////////////////////////////
    // Special println that prints configuration as well
    //////////////////////////////////////////////////
    public static void println(PrintStream out, String s, FeatureExpr ctx) {
        out.println("[" + ctx + "]: " + s);
    }
    public static void println(PrintStream out, int i, FeatureExpr ctx) {
        out.println("[" + ctx + "]: " + i);
    }
    public static void println(PrintStream out, Object o, FeatureExpr ctx) {
        out.println("[" + ctx + "]: " + o);
    }
    public static void println(PrintStream out, char c, FeatureExpr ctx) {
        out.println("[" + ctx + "]: " + c);
    }
    public static void println(PrintStream out, boolean b, FeatureExpr ctx) {
        out.println("[" + ctx + "]: " + b);
    }

    //////////////////////////////////////////////////
    // Truncating primitive types to int
    //////////////////////////////////////////////////
    public static Integer truncB(Integer o) {
        return (int) (byte) o.intValue();
    }
    public static Integer truncC(Integer o) {
        return (int) (char) o.intValue();
    }
    public static Integer truncZ(Integer o) {
        return (int) (byte) o.intValue();   // same as byte according to the spec of BASTORE
    }
    public static Integer truncS(Integer o) {
        return (int) (short) o.intValue();
    }

    public static V<Object> verifyAndThrowException(V<Object> e, FeatureExpr methodCtx) throws Throwable {
        V<Object> simplifiedV = e.simplified();
        if (simplifiedV.hasThrowable()) {
            if (simplifiedV instanceof One) {
                Throwable t = (Throwable) ((One) simplifiedV).getOne();
                FeatureExpr ctx = simplifiedV.getConfigSpace();
                if (!ctx.equivalentTo(methodCtx))
                    throw new RuntimeException("An exception/error was thrown under subcontext of method");
                throw t;
            } else {
                // must be a Choice
                throw new RuntimeException("An exception/error was thrown under subcontext of method");
            }
        } else {
            return e;
        }
    }

    /**
     * TODO: Needs to check whether we are lifting this class
     */
    public static Object newInstance(Class clazz, FeatureExpr ctx) throws Throwable {
        try {
            Constructor c = clazz.getConstructor(FeatureExpr.class);
            return c.newInstance(new Object[]{ctx});
        } catch (NoSuchMethodException e) {
            System.out.println("Could not find constructor with ctx");
            throw e;
        } catch (IllegalAccessException e) {
            System.out.println("Error initializing " + clazz.getName());
            throw e;
        } catch (InstantiationException e) {
            System.out.println("Error initializing " + clazz.getName());
            throw e;
        } catch (InvocationTargetException e) {
            System.out.println("Error initializing " + clazz.getName());
            throw e;
        }
    }


    /**
     * Replacement for BeanUtilsBean.copyProperty()
     *
     * Use reflection to find the corresponding set method and then invoke it.
     */
    public static void copyProperty(BeanUtilsBean bean, Object target, String key, Object value, FeatureExpr ctx) {
        Method[] methods = target.getClass().getMethods();
        String setterName = "set" + key.substring(0, 1).toUpperCase() + key.substring(1);
        for (int i = 0; i < methods.length; i++) {
            Method m = methods[i];
            // Lifted classes have parameter type embedded in method names, but it shouldn't matter
            // because standard JavaBean prohibits setters with the same names but different types
            if (m.getName().startsWith(setterName)) {
                // TODO: convert value to appropriate types if necessary
                V vValue;
                if (setterName.equals("setTabWidth")) {
                    Integer iValue = Integer.valueOf((String) value);
                    vValue = V.one(ctx, iValue);
                } else {
                    vValue = V.one(ctx, value);
                }
                try {
                    m.invoke(target, vValue, ctx);
                } catch (IllegalAccessException e) {
                    e.printStackTrace();
                } catch (InvocationTargetException e) {
                    e.printStackTrace();
                }
            }
        }
    }

    /**
     * Get the argument of parameterized type instead of V
     */
    public static Class getType(Field f) {
        Type t = f.getGenericType();
        assert t instanceof ParameterizedTypeImpl : "Not a parameterized type";
        assert ((ParameterizedTypeImpl) t).getActualTypeArguments().length == 1 :
                "Parameterized type with more than one argument";
        return (Class) ((ParameterizedTypeImpl) t).getActualTypeArguments()[0];
    }

    public static int getInt(Field f, Object obj) throws IllegalAccessException {
        try {
            Object value = f.get(obj);
            assert value instanceof One :
                    "Value of field " + f.getName() + " in " + obj + " is not V.One";
            Object actualValue = ((One) value).getOne();
            assert actualValue instanceof Integer :
                    "Actual value of field " + f.getName() + "in " + obj + " is not Integer";
            return (Integer) actualValue;
        } catch (IllegalAccessException e) {
            e.printStackTrace();
            throw e;
        }
    }

    /**
     * Assume we are getting constructor from a LIFTED class
     *
     * TODO: check that we are actually lifting this class
     */
    public static Constructor getConstructor(Class c, Class[] typeArgs) throws NoSuchMethodException {
        int numArgs = typeArgs.length;
        Class[] newTypeArgs = new Class[numArgs * 2 + 1];
        for (int i = 0; i < numArgs; i++) {
            newTypeArgs[i] = V.class;
            newTypeArgs[i + numArgs + 1] = typeArgs[i];
        }
        newTypeArgs[numArgs] = FeatureExpr.class;
        try {
            return c.getConstructor(newTypeArgs);
        } catch (NoSuchMethodException e) {
            System.err.println("Exception in VOps");
            throw e;
        }
    }

    public static Object newInstance(Constructor c, Object[] args, FeatureExpr ctx) throws InstantiationException, IllegalAccessException, InvocationTargetException {
        int argsCount = args.length;
        Object[] newArgs = new Object[argsCount * 2 + 1];
        for (int i = 0; i < argsCount; i++) {
            newArgs[i] = V.one(ctx, args[i]);
            newArgs[i + args.length + 1] = null;
        }
        newArgs[argsCount] = ctx;
        try {
            return c.newInstance(newArgs);
        } catch (InstantiationException e) {
            System.err.println("Exception in VOps");
            throw e;
        } catch (IllegalAccessException e) {
            System.err.println("Exception in VOps");
            throw e;
        } catch (InvocationTargetException e) {
            System.err.println("Exception in VOps");
            throw e;
        }
    }
}
