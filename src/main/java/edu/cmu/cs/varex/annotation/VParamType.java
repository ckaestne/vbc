package edu.cmu.cs.varex.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * used for library functions that support variability.
 * the first parameter is the context, all other parameters are variational, so is the return type.
 *
 * can be used on the method for the return type
 */
@Target({ElementType.PARAMETER,ElementType.METHOD})
@Retention(RetentionPolicy.RUNTIME)
public @interface VParamType {
  Class<?> value();
}

