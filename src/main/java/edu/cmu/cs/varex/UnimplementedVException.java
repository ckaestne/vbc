package edu.cmu.cs.varex;

/**
 * Created by ckaestne on 11/28/2015.
 */
public class UnimplementedVException extends RuntimeException {
  public UnimplementedVException(String note) {
    super("Transformation to variational execution not completed: "+note);
  }
  public UnimplementedVException() {
    super("Transformation to variational execution not completed");
  }
}
