package edu.cmu.cs.vbc.prog;

import edu.cmu.cs.varex.annotation.VConditional;


public class Test1 {
  @VConditional
  public boolean A;
  @VConditional
  public boolean B;

  public static void main(String[] args) {
    new Test1().run();
  }

  private void run() {
    int a = 3;
    if (A)
      a = a + 200;
    if (B)
      a = a + 5;

    int b = 0;
    for (int i = 0; i < a; i++) {
      b = b + 1;
    }
    System.out.println(b);
  }

}
