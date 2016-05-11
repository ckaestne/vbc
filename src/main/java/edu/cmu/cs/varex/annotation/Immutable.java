package edu.cmu.cs.varex.annotation;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * Mark immutable classes
 *
 * Usually if all fields are final, that class is immutable
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface Immutable {

}