package edu.cmu.cs.vbc.prog.bankaccount2;

/**
 * @author chupanw
 */
public class Account {
    public int OVERDRAFT_LIMIT = FM.overdraft ? -5000 : 0;

    public int balance = 0;

    Account() {
        if (FM.bankaccount) {
        }
    }

    private boolean update__wrappee__BankAccount(int x) {
        int newBalance = balance + x;
        if (newBalance < OVERDRAFT_LIMIT)
            return false;
        balance = balance + x;
        return true;
    }

    private boolean update__wrappee__DailyLimit(int x) {
        if (!FM.dailylimit)
            return update__wrappee__BankAccount(x);
        int newWithdraw = withdraw;
        if (x < 0) {
            newWithdraw += x;
            if (newWithdraw < DAILY_LIMIT)
                return false;
        }
        if (!update__wrappee__BankAccount(x))
            return false;
        withdraw = newWithdraw;
        return true;
    }

    boolean update(int x) {
        if (!FM.logging)
            return update__wrappee__DailyLimit(x);
        if (update__wrappee__DailyLimit(x)) {
            return true;
        }
        return false;

    }

    private boolean undoUpdate__wrappee__BankAccount(int x) {
        int newBalance = balance - x;
        if (newBalance < OVERDRAFT_LIMIT)
            return false;
        balance = newBalance;
        return true;
    }

    private boolean undoUpdate__wrappee__DailyLimit(int x) {
        if (!FM.dailylimit)
            return undoUpdate__wrappee__BankAccount(x);
        int newWithdraw = withdraw;
        if (x < 0) {
            newWithdraw -= x;
            if (newWithdraw < DAILY_LIMIT)
                return false;
        }
        if (!undoUpdate__wrappee__BankAccount(x))
            return false;
        withdraw = newWithdraw;
        return true;
    }

    boolean undoUpdate(int x) {
        if (!FM.logging)
            return undoUpdate__wrappee__DailyLimit(x);
        if (undoUpdate__wrappee__DailyLimit(x)) {
            return true;
        }
        return false;

    }

    public static int DAILY_LIMIT = -1000;

    public int withdraw = 0;

    static int INTEREST_RATE = 2;

    int interest = 0;

    int calculateInterest() {
        return balance * INTEREST_RATE / 36500;
    }

    int estimatedInterest(int daysLeft) {
        return interest + daysLeft * calculateInterest();
    }

    boolean credit(int amount) {
        return balance >= amount;
    }

    public boolean lock = false;

    void lock() {
        lock = true;
    }

    void unLock() {
        lock = false;
    }

    boolean isLocked() {
        return lock;
    }

}
