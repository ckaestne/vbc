package edu.cmu.cs.vbc.prog.bankaccount2;

/**
 * @author chupanw
 */
public class Application {
    public Account account = new Account();

    private void nextDay__wrappee__BankAccount() {
    }

    private void nextDay__wrappee__DailyLimit() {
        if (!FM.dailylimit) {
            nextDay__wrappee__BankAccount();
            return;
        }
        nextDay__wrappee__BankAccount();
        account.withdraw = 0;
    }

    void nextDay() {
        if (!FM.interest) {
            nextDay__wrappee__DailyLimit();
            return;
        }
        nextDay__wrappee__DailyLimit();
        account.interest += account.calculateInterest();
    }

    private void nextYear__wrappee__BankAccount() {
    }

    void nextYear() {
        if (!FM.interest) {
            nextYear__wrappee__BankAccount();
            return;
        }
        nextYear__wrappee__BankAccount();
        account.balance += account.interest;
        account.interest = 0;
    }

}
