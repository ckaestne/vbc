package edu.cmu.cs.vbc.prog.bankaccount2;

/**
 * @author chupanw
 */
public class Main {
    public static void main(String[] args) {
        new Main();
    }

    private Application a;

    private Application b;

    public Main() {
        a = new Application();
        b = new Application();
        if (FM.bankaccount) {
            getA().account.update(100);
            getB().account.update(200);
            getA().nextDay();
            getB().nextYear();
        }

        if (FM.creditworthiness) {
            getA().account.credit(10);
            getB().account.credit(20);
        }

        if (FM.transaction) {
            new Transaction().transfer(getA().account, getB().account, 30);
            new Transaction().transfer(getB().account, getA().account, 100);
        }

        System.out.println(a.account.balance);
        System.out.println(b.account.balance);
    }

    private Application getA() {
        return a;
    }

    private Application getB() {
        return b;
    }
}
