package model.java.util;

import de.fosd.typechef.featureexpr.FeatureExpr;
import edu.cmu.cs.varex.V;
import edu.cmu.cs.vbc.utils.Profiler;

import java.util.function.Function;

/**
 * @author chupanw
 */
public class ArrayList implements List {

    private V<? extends MyArrayList> vActual;
    private MyArrayList actual;

    /**
     * Split vActual LinkedLists according to current ctx
     */
    private void split(FeatureExpr ctx) {
        V<? extends MyArrayList> selected = vActual.smap(ctx, (Function<MyArrayList, MyArrayList>) t -> new MyArrayList(t));
        vActual = V.choice(ctx, selected, vActual);
    }

    @Override
    public V<? extends MyArrayList> getVCopies(FeatureExpr ctx) {
        return vActual.smap(ctx, l -> new MyArrayList(l));
    }

    public V<?> getVOfArrays(Class c, FeatureExpr ctx) {
        return vActual.smap(ctx, l -> {
            Object[] o = l.toArray();
            return java.util.Arrays.copyOf(o, l.size(), c);
        });
    }

    //////////////////////////////////////////////////
    // Non-V methods
    //////////////////////////////////////////////////
    public ArrayList() {
        actual = new MyArrayList();
    }

    public boolean add(Object o) {
        return actual.add(o);
    }

    //////////////////////////////////////////////////
    // V methods
    //////////////////////////////////////////////////

    public ArrayList(FeatureExpr ctx) {
        vActual = V.one(ctx, new MyArrayList());
    }

    public ArrayList(V<Integer> size, FeatureExpr ctx, int dummy) {
        vActual = size.smap(ctx, i -> new MyArrayList(i));
    }

    public ArrayList(V<Collection> vc, FeatureExpr ctx, Collection dummy) {
        vActual = vc.sflatMap(ctx, new Function<Collection, V<? extends MyArrayList>>() {
            @Override
            public V<? extends MyArrayList> apply(Collection collection) {
                return (V<? extends MyArrayList>) collection.getVCopies(ctx);
            }
        });
    }

    public V<?> add__Ljava_lang_Object__Z(V<?> elem, FeatureExpr ctx) {
        String id = "ArrayList#add#";
        Profiler.startTimer(id);
        V res = elem.sflatMap(ctx, (fe, e) -> {
            split(fe);
            return vActual.smap(fe, (featureExpr, l) -> l.add(e));
        });
        Profiler.stopTimer(id);
        return res;
    }

    public V<?> size____I(FeatureExpr ctx) {
        String id = "ArrayList#size#";
        Profiler.startTimer(id);
        V res = vActual.smap(ctx, list -> list.size());
        Profiler.stopTimer(id);
        return res;
    }

    public V<?> get__I__Ljava_lang_Object(V<? extends Integer> index, FeatureExpr ctx) {
        String id = "ArrayList#get#";
        Profiler.startTimer(id);
        V res = vActual.sflatMap(ctx, (fe, list) -> index.smap(fe, i -> list.get(i.intValue())));
        Profiler.stopTimer(id);
        return res;
    }

    public V<?> sort__Lmodel_java_util_Comparator__V(V<Comparator> vComparator, FeatureExpr ctx) {
        String id = "ArrayList#sort#";
        Profiler.startTimer(id);
        vComparator.sforeach(ctx, (fe, c) -> {
            split(fe);
            vActual.sforeach(fe, l -> l.sort(c::compare));
        });
        Profiler.stopTimer(id);
        return null;
    }

    public V<?> isEmpty____Z(FeatureExpr ctx) {
        String id = "ArrayList#isEmpty#";
        Profiler.startTimer(id);
        V res = vActual.smap(ctx, list -> list.isEmpty());
        Profiler.stopTimer(id);
        return res;
    }

    public V<? extends java.util.Iterator> iterator____Ljava_util_Iterator(FeatureExpr ctx) {
        String id = "ArrayList#iterator#";
        Profiler.startTimer(id);
        V res = vActual.smap(ctx, l -> l.iterator());
        Profiler.stopTimer(id);
        return res;
    }

    public V<? extends Boolean> remove__Ljava_lang_Object__Z(V<?> vo, FeatureExpr ctx) {
        String id = "ArrayList#remove#";
        Profiler.startTimer(id);
        V res = vo.sflatMap(ctx, (fe, o) -> {
            split(fe);
            return vActual.smap(fe, l -> l.remove(o));
        });
        Profiler.stopTimer(id);
        return res;
    }

    public V<?> clear____V(FeatureExpr ctx) {
        String id = "ArrayList#clear#";
        Profiler.startTimer(id);
        split(ctx);
        vActual.sforeach(ctx, l -> l.clear());
        Profiler.stopTimer(id);
        return null;
    }

    @Override
    public V<?> contains__Ljava_lang_Object__Z(V<?> vO, FeatureExpr ctx) {
        String id = "ArrayList#contains#";
        Profiler.startTimer(id);
        V res = vActual.sflatMap(ctx, (fe, l) -> vO.smap(fe, o -> l.contains(o)));
        Profiler.stopTimer(id);
        return res;
    }

    public V<?> set__I_Ljava_lang_Object__Ljava_lang_Object(V<Integer> vI, V<?> vO, FeatureExpr ctx) {
        String id = "ArrayList#set#";
        Profiler.startTimer(id);
        V res = vI.sflatMap(ctx, (fe, i) -> vO.sflatMap(fe, (fe2, o) -> {
            split(fe2);
            return vActual.smap(fe2, (fe3, l) -> l.set(i, o));
        }));
        Profiler.stopTimer(id);
        return res;
    }

    /**
     * Probably not efficient, but we need to wrap elements into Vs.
     *
     * perf: maybe use System.arrayCopy for larger arrays?
     */
    public V toArray__Array_Ljava_lang_Object__Array_Ljava_lang_Object(V<V[]> a, FeatureExpr ctx) {
        return a.sflatMap(ctx, (fe, aa) -> vActual.smap(fe, (fe2, l) -> {
            Object[] elements = l.toArray();
            V[] destArray = aa;
            if (aa.length < elements.length) {
                destArray = new V[elements.length];
            }
            for (int i = 0; i < elements.length; i++) {
                destArray[i] = V.one(fe2, elements[i]);
            }
            return destArray;
        }));
    }
}

/**
 * Override equals() for splitting
 */
class MyArrayList extends java.util.ArrayList {
    MyArrayList(int size) {
        super(size);
    }
    MyArrayList() {
        super();
    }
    MyArrayList(MyArrayList origin) {
        super(origin);
    }
    @Override
    public boolean equals(Object o) {
        return o == this;
    }
}
