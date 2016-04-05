package edu.cmu.cs.varex.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * marks functions that are left in a nonvariational form
 * during the transformation to the variational interpreter.
 * <p>
 * there is a corresponding newer function that takes variational
 * arguments or context parameters, that should be used instead.
 * <p>
 * deprecated functions are helper functions to call the correct
 * variational functions. they should be final
 */
@Target({ElementType.METHOD, ElementType.CONSTRUCTOR})
@Retention(RetentionPolicy.RUNTIME)
public @interface VDeprecated {
    /**
     * value hints at the correct alternative variational implementation
     *
     * @return
     */
    String value() default "";
}

