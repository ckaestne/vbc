package model.java.util;

import de.fosd.typechef.featureexpr.FeatureExpr;
import edu.cmu.cs.varex.V;
import model.Contexts;

import java.util.Collection;
import java.util.function.BiFunction;
import java.util.function.Function;

/**
 * @author chupanw
 */
public class LinkedList implements List {

    public LinkedList(FeatureExpr ctx) {
        vActual = V.one(ctx, new MyLinkedList());
    }

    private V<? extends MyLinkedList> vActual;

    /**
     * Split vActual LinkedLists according to current ctx
     */
    private void split(FeatureExpr ctx) {
        V<? extends MyLinkedList> selected = vActual.smap(ctx, (Function<MyLinkedList, MyLinkedList>) t -> new MyLinkedList(t));
        vActual = V.choice(ctx, selected, vActual);
    }

    public V<?> add__Ljava_lang_Object__Z(V<?> elem, FeatureExpr ctx) {
        split(ctx);
        return vActual.sflatMap(ctx, (BiFunction<FeatureExpr, MyLinkedList, V<?>>) (fe, list) ->
                elem.sflatMap(fe, (BiFunction<FeatureExpr, Object, V<? extends Boolean>>) (featureExpr, o) ->
                        V.one(featureExpr, list.add(o))
                ) );
    }

    @Override
    public V<?> size____I(FeatureExpr ctx) {
        return vActual.smap(ctx, MyLinkedList::size);
    }

    @Override
    public V<?> get__I__Ljava_lang_Object(V<? extends Integer> index, FeatureExpr ctx) {
        return vActual.sflatMap(ctx, (fe, list) -> index.smap(fe, i -> list.get(i)));
    }

    @Override
    public V<?> sort__Lmodel_java_util_Comparator__V(V<Comparator> vComparator, FeatureExpr ctx) {
        split(ctx);
        vActual.sforeach(ctx, (fe, list) -> vComparator.sforeach(fe, (fe2, comparator) -> {
            Contexts.model_java_util_Comparator_compare = fe2;
            list.sort(comparator::compare);
        }));
        return null;    // dummy value, will never use
    }

    @Override
    public V<?> isEmpty____Z(FeatureExpr ctx) {
        return vActual.smap(ctx, list -> list.isEmpty());
    }

    @Override
    public V<?> iterator____Ljava_util_Iterator(FeatureExpr ctx) {
        return vActual.smap(ctx, l -> l.iterator());
    }

    public V<?> addFirst__Ljava_lang_Object__V(V<?> vElem, FeatureExpr ctx) {
        split(ctx);
        vActual.sforeach(ctx, (fe, list) -> vElem.sforeach(fe, (fe2, e) -> list.addFirst(e)));
        return null;    // dummy value, will never use
    }

    public V<?> removeFirst____Ljava_lang_Object(FeatureExpr ctx) {
        split(ctx);
        return vActual.smap(ctx, list -> list.removeFirst());
    }

    public V<?> indexOf__Ljava_lang_Object__I(V<?> vElem, FeatureExpr ctx) {
        return vActual.sflatMap(ctx, (fe, list) -> vElem.smap(fe, (fe2, e) -> list.indexOf(e)));
    }


    public V<?> remove__I__Ljava_lang_Object(V<? extends Integer> vIndex, FeatureExpr ctx) {
        split(ctx);
        return vActual.sflatMap(ctx, (fe, list) -> vIndex.smap(fe, i -> list.remove(i.intValue())));
    }


    public V<?> add__I_Ljava_lang_Object__V(V<? extends Integer> vIndex, V<?> vElem, FeatureExpr ctx) {
        split(ctx);
        vActual.sforeach(ctx, (fe, list) ->
                vIndex.sforeach(fe, (fe2, i) ->
                        vElem.sforeach(fe2, e ->
                                list.add(i.intValue(), e)
                        )
                )
        );
        return null;    // dummy value, will never use
    }

    public V<?> clear____V(FeatureExpr ctx) {
        split(ctx);
        vActual.sforeach(ctx, java.util.LinkedList::clear);
        return null;    // dummy value, will never use
    }

    @Override
    public V<?> contains__Ljava_lang_Object__Z(V<?> vO, FeatureExpr ctx) {
        return vActual.sflatMap(ctx, (fe, l) -> vO.smap(fe, o -> l.contains(o)));
    }

    @Override
    public V<?> remove__Ljava_lang_Object__Z(V<?> vO, FeatureExpr ctx) {
        return vActual.sflatMap(ctx, (fe, l) -> vO.smap(fe, o -> l.remove(o)));
    }

    @Override
    public V<?> getVCopies(FeatureExpr ctx) {
        throw new RuntimeException("Not implemented");
    }
}

class MyLinkedList extends java.util.LinkedList {
    MyLinkedList(Collection c) {
        super(c);
    }
    MyLinkedList() {
        super();
    }
    @Override
    public boolean equals(Object obj) {
        return obj == this;
    }
}
