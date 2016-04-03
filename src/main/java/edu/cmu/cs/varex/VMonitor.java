package edu.cmu.cs.varex;

import de.fosd.typechef.featureexpr.FeatureExpr;

import javax.annotation.Nonnull;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * variational synchronization mechanism
 * <p>
 * the VMonitor API mirrors the working of the JVM's monitor mechanism:
 * when entering from the same thread, a counter is increased, when entering
 * from a different thread with the counter >0, it blocks until the counter
 * is 0 again; when exiting the monitor as its owner the counter is decreased
 * <p>
 * see https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-6.html#jvms-6.5.monitorenter
 */
public class VMonitor {

    private static ConcurrentMap<Object, ConcurrentMap<FeatureExpr, Object>> locks = new ConcurrentHashMap<>();

    /**
     * enters the monitor associated with object `obj` under condition `ctx`.
     * returns the condition under which the monitor could be entered; blocks only
     * if it cannot be entered under any subcondition of `ctx`.
     * (the possibility of entering under partial conditions means that the caller
     * needs to check the return type and split the control flow variationally,
     * recalling vmonitorenter for the conditions not entered yet)
     * <p>
     * intents to behave as the equivalent to the byte code instruction `monitorenter`
     */
    public static FeatureExpr vmonitorenter(@Nonnull Object obj, @Nonnull FeatureExpr ctx) {
        assert obj != null;
        assert ctx != null;

        ConcurrentMap<FeatureExpr, Object> locksForObj = locks.get(obj);

        // if there isn't any lock under any condition or any condition that overlaps
        // with our condition, we can just create one under our condition

        // if we are lucky, the lock exists already exactly under the same condition,
        // then we can just use the existing lock; once we have acquired the lock
        // we need to check whether the lock object still exists though, it may have
        // been removed due to a split in the meantime, in which case we need to repeat
        // the process (recursive call)

        // if our condition is a subcondition of an existing condition:
        // attempt to lock under that previous condition (we cannot do anything before
        // that lock is release anyway), but once we acquire that lock, release and remove it
        // immediately and lock under our own condition; if there are others waiting for
        // the same lock they will now also be released and reevaluated

        // in all other cases, we are partially overlapping. we can just operate
        // on the condition that is not overlapping and acquire a lock for that condition
        // (the method contract assures that the caller has to retry with the remaining
        // condition)

        return ctx;
    }

    /**
     * leaves the monitor associated with `obj` under condition `ctx`.
     * <p>
     * intents to behave as the equivalent to the byte code instruction `monitorexit`
     */
    public static void vmonitorexit(@Nonnull Object obj, @Nonnull FeatureExpr ctx) {
        assert obj != null;
        assert ctx != null;

    }


    private static void monitorenter(Object obj) {
        //forward to original byte code instructions
    }

    private static void monitorexit(Object obj) {
        //forward to original byte code instructions
    }

}


/**
 * notes:
 * <p>
 * synchronization of vmonitorenter will be tricky, because we need to synchronize
 * access to the locks map, while potentially blocking on waiting for a monitor. we
 * need to make sure that we do not introduce global blocking. also we should probably
 * avoid that all locking goes through a single `locks` map, but possibly rather
 * instrument the `Object` class to have a field with a list of locks.
 * <p>
 * the behavior above will break the Thread.holdsLock method. may potentially be fixed
 * by a model class.
 * <p>
 * it seems like we now need to synchronize on every single PUT operation, because we are
 * getting the old value first. Two put operations in mutually exclusive contexts could
 * otherwise corrupt each other.
 * <p>
 * it seems unlikely that we will be able to deal with variational execution of threads.
 * we can ensure that a thread will never be executed in a context wider than the context
 * in which it was started. the thread's interrupt status is hardcoded (native); it seems
 * unlikely that we can interrupt a thread variationally and variationally throw interrupt
 * exceptions on sleep, wait, and join.
 * <p>
 * we may have to except that two threads started in mutually exclusive contexts cannot
 * be assumed to be independent without modifying the JVM.
 */