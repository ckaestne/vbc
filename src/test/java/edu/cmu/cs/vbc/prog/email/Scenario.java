package edu.cmu.cs.vbc.prog.email;

public class Scenario {
	public static boolean abortedRun = false;

	public static void test() {

		Test_Actions.setup();
		Test_Actions.bobKeyAdd();
		Test_Actions.bobSetAddressBook();
		// Test_Actions.bobToRjh();
		Test_Actions.bobToAlias();
	}

	public static void main(String[] args) {
		if (args != null && args.length > 0) {
			FeatureSwitches.init(args);
		}

//		long start = System.nanoTime();
		new Scenario();
//		long end = System.nanoTime();
//		System.out.println("TIME:" + (end - start));
		
	}

	public Scenario() {
		Test_Actions.setup();
		
		Test_Actions.bobKeyAdd();
		Test_Actions.bobKeyAddChuck();
		Test_Actions.bobKeyChange();
		Test_Actions.bobSetAddressBook();
		Test_Actions.chuckKeyAdd();
		Test_Actions.rjhDeletePrivateKey();
		Test_Actions.rjhEnableForwarding();
		Test_Actions.rjhKeyAdd();
		Test_Actions.rjhKeyAddChuck();
		Test_Actions.rjhKeyChange();
		Test_Actions.rjhSetAutoRespond();
		Test_Actions.bobToAlias();
		Test_Actions.bobToRjh();
		Test_Actions.rjhToBob();
	}

}