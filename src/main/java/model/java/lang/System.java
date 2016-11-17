package model.java.lang;

/**
 * Trigger expanding array by modifying arraycopy signature.
 *
 * @author chupanw
 */
public class System {
    public static void arraycopy(Object[] src, int srcPos, Object[] dest, int destPos, int length) {
        java.lang.System.arraycopy(src, srcPos, dest, destPos, length);
    }
}
