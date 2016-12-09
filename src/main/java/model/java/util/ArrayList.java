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
        split(ctx);
        V res = vActual.sflatMap(ctx, (fe, list) -> elem.sflatMap(fe, (featureExpr, o) -> V.one(featureExpr, list.add(o)) ) );
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
        split(ctx);
        vActual.sforeach(ctx, (fe, list) -> vComparator.sforeach(fe, c -> list.sort(c::compare)));
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
        split(ctx);
        V res = vActual.sflatMap(ctx, (fe, l) -> vo.smap(fe, o -> l.remove(o)));
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
        split(ctx);
        V res = vActual.sflatMap(ctx, (fe1, l) -> vI.sflatMap(fe1, (fe2, i) -> vO.smap(fe2, o -> l.set(i, o))));
        Profiler.stopTimer(id);
        return res;
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
