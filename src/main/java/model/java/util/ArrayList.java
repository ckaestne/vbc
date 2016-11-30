package model.java.util;

import de.fosd.typechef.featureexpr.FeatureExpr;
import edu.cmu.cs.varex.V;

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
        split(ctx);
        return vActual.sflatMap(ctx, (fe, list) -> elem.sflatMap(fe, (featureExpr, o) -> V.one(featureExpr, list.add(o)) ) );
    }

    public V<?> size____I(FeatureExpr ctx) {
        return vActual.smap(ctx, list -> list.size());
    }

    public V<?> get__I__Ljava_lang_Object(V<? extends Integer> index, FeatureExpr ctx) {
        return vActual.sflatMap(ctx, (fe, list) -> index.smap(fe, i -> list.get(i.intValue())));
    }

    public V<?> sort__Lmodel_java_util_Comparator__V(V<Comparator> vComparator, FeatureExpr ctx) {
        split(ctx);
        vActual.sforeach(ctx, (fe, list) -> vComparator.sforeach(fe, c -> list.sort(c::compare)));
        return null;
    }

    public V<?> isEmpty____Z(FeatureExpr ctx) {
        return vActual.smap(ctx, list -> list.isEmpty());
    }

    public V<? extends java.util.Iterator> iterator____Ljava_util_Iterator(FeatureExpr ctx) {
        return vActual.smap(ctx, l -> l.iterator());
    }

    public V<? extends Boolean> remove__Ljava_lang_Object__Z(V<?> vo, FeatureExpr ctx) {
        split(ctx);
        return vActual.sflatMap(ctx, (fe, l) -> vo.smap(fe, o -> l.remove(o)));
    }

    public V<?> clear____V(FeatureExpr ctx) {
        split(ctx);
        vActual.sforeach(ctx, l -> l.clear());
        return null;
    }

    @Override
    public V<?> contains__Ljava_lang_Object__Z(V<?> vO, FeatureExpr ctx) {
        return vActual.sflatMap(ctx, (fe, l) -> vO.smap(fe, o -> l.contains(o)));
    }

    public V<?> set__I_Ljava_lang_Object__Ljava_lang_Object(V<Integer> vI, V<?> vO, FeatureExpr ctx) {
        split(ctx);
        return vActual.sflatMap(ctx, (fe1, l) -> vI.sflatMap(fe1, (fe2, i) -> vO.smap(fe2, o -> l.set(i, o))));
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
