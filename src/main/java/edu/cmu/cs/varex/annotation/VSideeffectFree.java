package edu.cmu.cs.varex.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * can be used to mark library functions that are known to
 * be free of side effects and thus can be called repeatedly
 * in different contexts
 */
@Target({ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
public @interface VSideeffectFree {
}
