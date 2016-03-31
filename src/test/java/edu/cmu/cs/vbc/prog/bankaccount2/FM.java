package edu.cmu.cs.vbc.prog.bankaccount2;

import edu.cmu.cs.varex.annotation.VConditional;

/**
 * @author chupanw
 */
public class FM {
    @VConditional
    public static boolean bankaccount;
    @VConditional
    public static boolean creditworthiness;
    @VConditional
    public static boolean dailylimit;
    @VConditional
    public static boolean interest;
    @VConditional
    public static boolean interestestimation;
    @VConditional
    public static boolean lock;
    @VConditional
    public static boolean logging;
    @VConditional
    public static boolean overdraft;
    @VConditional
    public static boolean transaction;
    @VConditional
    public static boolean transactionlog;

    public static boolean valid() {
        return bankaccount && (!dailylimit || bankaccount) && (!interest || bankaccount) && (!overdraft || bankaccount) && (!creditworthiness || bankaccount) && (!lock || bankaccount) && (!logging || bankaccount) && (!interestestimation || interest) && (!transaction || lock) && (!transactionlog || logging) && (!logging || !transaction || transactionlog) && (!transactionlog || logging) && (!transactionlog || transaction);
    }
}
