package edu.cmu.cs.vbc.prog.BankAccount;

/**
 * Simplify so that there is no exceptions handling
 * @author chupanw
 */
public class Main {
    public static void main(String[] args) {
        System.out.println("BankAccount");
        Application a = new Application();
        Application b = new Application();

        a.account.update(100);
        System.out.println("A Balance = " + a.account.balance);
        a.account.undoUpdate(100);
        System.out.println("A Balance = " + a.account.balance);

        a.account.update(200);
        b.account.update(400);
        a.nextDay();
        b.nextYear();

        // Interest Estimation Feature
        a.account.estimatedInterest(10);

//        // Credit Worthiness Feature
//        a.account.credit(10);
//        b.account.credit(20);

        // Interest Feature
        a.account.update(1);
        b.account.update(1);

        System.out.println("A");
        System.out.println(a.account.balance);
        System.out.println("B");
        System.out.println(b.account.balance);

        // Transaction Feature
        System.out.println("transaction");
        System.out.println("A");
        System.out.println(a.account.balance);
        (new Transaction()).transfer(a.account, b.account, 30);
        (new Transaction()).transfer(b.account, a.account, -30);
        (new Transaction()).transfer(a.account, b.account, 140);
        (new Transaction()).transfer(a.account, b.account, 1000);

        System.out.println("A");
        System.out.println(a.account.balance);
        System.out.println("B");
        System.out.println(b.account.balance);
    }
}
