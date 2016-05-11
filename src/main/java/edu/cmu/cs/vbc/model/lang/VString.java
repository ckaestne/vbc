package edu.cmu.cs.vbc.model.lang;

import de.fosd.typechef.featureexpr.FeatureExpr;
import edu.cmu.cs.varex.V;
import edu.cmu.cs.varex.annotation.Immutable;
import scala.Int;

/**
 * @author chupanw
 */
@Immutable
public class VString extends VObject {

    public String actual;

    public VString(String s) {
        actual = s;
    }

    public VString(VCharacter value[]) {
        char v[] = new char[value.length];
        for (int i = 0; i < value.length; i++) {
            if (value[i] != null)
                v[i] = value[i].charValue();
        }
        actual = new String(v);
    }

    @Override
    public String toString() {
        return actual.toString();
    }

    public static VString valueOf(String s) {
        return new VString(s);
    }

    public VString trim(FeatureExpr ctx) {
        return new VString(actual.trim());
    }

    @Override
    public boolean equals(Object obj) {
        if (obj instanceof VString) {
            return actual.equals(((VString) obj).actual);
        }
        else {
            return false;
        }
    }

    //////////////////////////////////////////////////
    // V methods
    //////////////////////////////////////////////////
    // TODO: vca should be VCharacter, not VInteger
    public static V<? extends VString> Vinit$Array_C$V(V<V<VInteger>[]> vca, FeatureExpr ctx) {
        return vca.smap(ctx, x -> {
            //TODO: Temporarily assume that all elements in array are Ones.
            VCharacter vc[] = new VCharacter[x.length];
            for (int i = 0; i < x.length; i++) {
                if (x[i] != null) {
                    vc[i] = new VCharacter((char)x[i].getOne().actual.intValue());
                }
            }
            return new VString(vc);
        });
    }

    public static V<? extends VString> valueOf$I$Ljava_lang_String(V<? extends VInteger> vi, FeatureExpr ctx) {
        return vi.smap(ctx, i -> {
            return new VString(i.actual.toString());
        });
    }

    public static V<? extends VString> Vinit$Ljava_lang_String$V(V<? extends VString> vs, FeatureExpr ctx) {
        return vs.smap(ctx, x -> new VString(x.actual));
    }

    public V<? extends VString> substring$II$Ljava_lang_String(V<? extends VInteger> beginIdx, V<? extends VInteger> endIdx, FeatureExpr ctx) {
        return beginIdx.sflatMap(ctx, x -> {
            return endIdx.smap(ctx, y -> {
                return new VString(actual.substring(x.actual, y.actual));
            });
        });
    }
}
