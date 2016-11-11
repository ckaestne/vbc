package edu.cmu.cs.vbc.prog.email;

import java.util.List;

public  class  PL_Interface_impl {

	// trigger <init> of FeatureSwitches, so that options can be set while comparing traces.
	private FeatureSwitches fs = new FeatureSwitches();

	public static void main(String[] args) {
		try {
			(new PL_Interface_impl()).start(-1,2);
			System.out.println("no Exception");
		} catch (Throwable e) {
			System.out.println("Caught Exception: " + e.getClass() + " " + e.getMessage());
			e.printStackTrace();
		}
	}

	
	public void start(int specification, int variation) {
//		runRandomActions(variation);
		Scenario.test();
	}

	public List<String> getExecutedActions() {
		return Test_Actions.actionHistory;
	}

	public boolean isAbortedRun() {
		return Scenario.abortedRun;
	}

}
