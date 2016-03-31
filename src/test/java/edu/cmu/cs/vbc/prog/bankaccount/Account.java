package edu.cmu.cs.vbc.prog.bankaccount;

import edu.cmu.cs.varex.annotation.VConditional;

public class Account {
  final int OVERDRAFT_LIMIT = -5000;

  int balance = 0;

  @VConditional
  public boolean LUCKY;

  Account() {
    if (LUCKY) {
      balance = 1000;
    }
  }

  private boolean update__wrappee__BankAccount(int x) {
    int newBalance = balance + x;
    if (newBalance < OVERDRAFT_LIMIT)
      return false;
    balance = newBalance;
    return true;
  }

  boolean update(int x) {
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

  private boolean undoUpdate__wrappee__BankAccount(int x) {
    int newBalance = balance - x;
    if (newBalance < OVERDRAFT_LIMIT)
      return false;
    balance = newBalance;
    return true;
  }

  boolean undoUpdate(int x) {
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

  //TODO: handle static
  final /*static*/ int DAILY_LIMIT = -1000;

  int withdraw = 0;

  final /*static*/ int INTEREST_RATE = 2;

  int interest = 0;

  int calculateInterest() {
    return balance * INTEREST_RATE / 36500;
  }

  int estimatedInterest(int daysLeft) {
    return interest + daysLeft * calculateInterest();
  }

  // TODO: unbalanced stack here
  boolean credit(int amount) {
    return balance >= amount;
  }

  private boolean lock = false;

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


