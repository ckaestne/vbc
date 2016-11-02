package edu.cmu.cs.vbc.prog.email;
import java.util.ArrayList;
import java.util.List;

/**
 * This class implements the actions that can be executed by the test scenarios.
 * (setup() is always executed first to setup the characters and their private keys)
 * 
 * @author rhein
 */
public class Test_Actions {

	public static boolean executedUnimplementedAction = false;

	static Client bob, rjh, chuck;

	/*
	 * Filterfield is possible here.
	 * It causes some speedup, but no advantage for family, or product based verification.
	 * It might cause more problems than it helps.
	 */
	// @FilterField
	public static List<String> actionHistory = new ArrayList<String>();

	/**
	 * Bob gets Key of RJH
	 */
	public static void bobKeyAdd__before__keys() {
		actionHistory.add("bobKeyAdd_NOTIMPL");
		executedUnimplementedAction = true;
	}

	/**
	 * Bob gets Key of RJH
	 */
	public static void bobKeyAdd__role__keys() {
		actionHistory.add("bobKeyAdd");
		// unrealistic simplification. Private key == Public key
		bob.addKeyringEntry(rjh, rjh.getPrivateKey());
		Util.prompt("bob added rjhs key");
	}

	/**
	 * Bob gets Key of RJH
	 */
	public static void bobKeyAdd() {
		if (FeatureSwitches.__SELECTED_FEATURE_keys) {
			bobKeyAdd__role__keys();
		} else {
			bobKeyAdd__before__keys();
		}
	}

	/**
	 * Bob gets Key of Chuck
	 */
	public static void bobKeyAddChuck__before__keys() {
		actionHistory.add("bobKeyAddChuck_NOTIMPL");
		executedUnimplementedAction = true;
	}

	/**
	 * Bob gets Key of Chuck
	 */
	public static void bobKeyAddChuck__role__keys() {
		actionHistory.add("bobKeyAddChuck");
		// unrealistic simplification. Private key == Public key
		bob.addKeyringEntry(chuck, chuck.getPrivateKey());
	}

	/**
	 * Bob gets Key of Chuck
	 */
	public static void bobKeyAddChuck() {
		if (FeatureSwitches.__SELECTED_FEATURE_keys) {
			bobKeyAddChuck__role__keys();
		} else {
			bobKeyAddChuck__before__keys();
		}
	}

	public static void bobKeyChange__before__keys() {
		actionHistory.add("bobKeyChange_NOTIMPL");
		executedUnimplementedAction = true;
	}

	public static void bobKeyChange__role__keys() {
		actionHistory.add("bobKeyChange");
		Client.generateKeyPair(bob, 777);
	}

	public static void bobKeyChange() {
		if (FeatureSwitches.__SELECTED_FEATURE_keys) {
			bobKeyChange__role__keys();
		} else {
			bobKeyChange__before__keys();
		}
	}

	public static void bobSetAddressBook__before__addressbook() {
		actionHistory.add("bobSetAddressBook_NOTIMPL");
		executedUnimplementedAction = true;
	}

	public static void bobSetAddressBook__role__addressbook() {
		actionHistory.add("bobSetAddressBook");
		bob.addAddressbookEntry("AliasForRJHandChuck", rjh.getName());
		bob.addAddressbookEntry("AliasForRJHandChuck", chuck.getName());
	}

	public static void bobSetAddressBook() {
		if (FeatureSwitches.__SELECTED_FEATURE_addressbook) {
			bobSetAddressBook__role__addressbook();
		} else {
			bobSetAddressBook__before__addressbook();
		}
	}

	public static void bobToAlias__before__addressbook() {
		actionHistory.add("bobToAlias");
		executedUnimplementedAction = true;
	}

	public static void bobToAlias__role__addressbook() {
		if (bob.getAddressBookReceiversForAlias("AliasForRJHandChuck").isEmpty()) {
			actionHistory.add("AbortedBobToAlias");
			return;
		}
		actionHistory.add("bobToAlias");
		String subject = "Subject";
		String body = "Body";
		Client.sendEmail(bob, "AliasForRJHandChuck", subject, body);
	}

	public static void bobToAlias() {
		if (FeatureSwitches.__SELECTED_FEATURE_addressbook) {
			bobToAlias__role__addressbook();
		} else {
			bobToAlias__before__addressbook();
		}
	}

	public static void bobToRjh() {

		actionHistory.add("bobToRjh");
		String subject = "Subject";
		String body = "Body";
		Client.sendEmail(bob, rjh.getName(), subject, body);
	}

	/**
	 * Chuck gets Key of Bob
	 */
	public static void chuckKeyAdd__before__keys() {
		actionHistory.add("chuckKeyAdd_NOTIMPL");
		executedUnimplementedAction = true;
	}

	/**
	 * Chuck gets Key of Bob
	 */
	public static void chuckKeyAdd__role__keys() {
		actionHistory.add("chuckKeyAdd");
		chuck.addKeyringEntry(bob, bob.getPrivateKey());
	}

	/**
	 * Chuck gets Key of Bob
	 */
	public static void chuckKeyAdd() {
		if (FeatureSwitches.__SELECTED_FEATURE_keys) {
			chuckKeyAdd__role__keys();
		} else {
			chuckKeyAdd__before__keys();
		}
	}

	/**
	 * Delete RJHs private key
	 */
	public static void rjhDeletePrivateKey__before__keys() {
		actionHistory.add("rjhDeletePrivateKey_NOTIMPL");
		executedUnimplementedAction = true;
	}

	/**
	 * Delete RJHs private key
	 */
	public static void rjhDeletePrivateKey__role__keys() {
		actionHistory.add("rjhDeletePrivateKey");
		rjh.setPrivateKey(0);
	}

	/**
	 * Delete RJHs private key
	 */
	public static void rjhDeletePrivateKey() {
		if (FeatureSwitches.__SELECTED_FEATURE_keys) {
			rjhDeletePrivateKey__role__keys();
		} else {
			rjhDeletePrivateKey__before__keys();
		}
	}

	public static void rjhEnableForwarding__before__forward() {
		actionHistory.add("rjhEnableForwarding_NOTIMPL");
		executedUnimplementedAction = true;
	}

	public static void rjhEnableForwarding__role__forward() {
		actionHistory.add("rjhEnableForwarding");
		rjh.setForwardReceiver(chuck);
	}

	public static void rjhEnableForwarding() {
		if (FeatureSwitches.__SELECTED_FEATURE_forward) {
			rjhEnableForwarding__role__forward();
		} else {
			rjhEnableForwarding__before__forward();
		}
	}

	/**
	 * RJH gets Key of Bob
	 */
	public static void rjhKeyAdd__before__keys() {
		actionHistory.add("rjhKeyAdd_NOTIMPL");
		executedUnimplementedAction = true;
	}

	/**
	 * RJH gets Key of Bob
	 */
	public static void rjhKeyAdd__role__keys() {
		actionHistory.add("rjhKeyAdd");
		rjh.addKeyringEntry(bob, bob.getPrivateKey());
	}

	/**
	 * RJH gets Key of Bob
	 */
	public static void rjhKeyAdd() {
		if (FeatureSwitches.__SELECTED_FEATURE_keys) {
			rjhKeyAdd__role__keys();
		} else {
			rjhKeyAdd__before__keys();
		}
	}

	// actions
	/**
	 * RJH gets Key of Chuck
	 */
	public static void rjhKeyAddChuck__before__keys() {
		actionHistory.add("rjhKeyAddChuck_NOTIMPL");
		executedUnimplementedAction = true;
	}

	// actions
	/**
	 * RJH gets Key of Chuck
	 */
	public static void rjhKeyAddChuck__role__keys() {
		actionHistory.add("rjhKeyAddChuck");
		rjh.addKeyringEntry(chuck, chuck.getPrivateKey());
	}

	// actions
	/**
	 * RJH gets Key of Chuck
	 */
	public static void rjhKeyAddChuck() {
		if (FeatureSwitches.__SELECTED_FEATURE_keys) {
			rjhKeyAddChuck__role__keys();
		} else {
			rjhKeyAddChuck__before__keys();
		}
	}

	public static void rjhKeyChange__before__keys() {
		actionHistory.add("rjhKeyChange_NOTIMPL");
		executedUnimplementedAction = true;
	}

	public static void rjhKeyChange__role__keys() {
		actionHistory.add("rjhKeyChange");
		Client.generateKeyPair(rjh, 666);
	}

	public static void rjhKeyChange() {
		if (FeatureSwitches.__SELECTED_FEATURE_keys) {
			rjhKeyChange__role__keys();
		} else {
			rjhKeyChange__before__keys();
		}
	}

	public static void rjhSetAutoRespond__before__autoresponder() {
		actionHistory.add("rjhSetAutoRespond_NOTIMPL");
		executedUnimplementedAction = true;
	}

	public static void rjhSetAutoRespond__role__autoresponder() {
		actionHistory.add("rjhSetAutoRespond");
		rjh.setAutoResponse(true);
	}

	public static void rjhSetAutoRespond() {
		if (FeatureSwitches.__SELECTED_FEATURE_autoresponder) {
			rjhSetAutoRespond__role__autoresponder();
		} else {
			rjhSetAutoRespond__before__autoresponder();
		}
	}

	public static void rjhToBob() {
		actionHistory.add("rjhToBob");
		String subject = "subject";
		String body = "body";
		Client.sendEmail(rjh, bob.getName(), subject, body);
	}

	public static void setup() {
		actionHistory.add("setup");
//		SpecificationManager.setupSpecifications();
		Client.resetClients();

		bob = Client.createClient("bob");
		setup_bob(bob);
		Util.prompt("bob: " + bob.getName() + "(ID: " + String.valueOf(bob.getId()) + ")");
		// rjh = createClient("rjh");
		rjh = Client.createClient("rjh");
		setup_rjh(rjh);
		Util.prompt("rjh: " + rjh.getName() + "(ID: " + String.valueOf(rjh.getId()) + ")");
		// chuck = createClient("chuck");
		chuck = Client.createClient("chuck");
		setup_chuck(chuck);
		Util.prompt("chuck: " + chuck.getName() + "(ID: " + String.valueOf(chuck.getId()) + ")");
	}

	public static void setup_bob__before__keys(Client bob) {}

	public static void setup_bob__role__keys(Client bob) {
		bob.setPrivateKey(123);
	}

	public static void setup_bob(Client bob) {
		if (FeatureSwitches.__SELECTED_FEATURE_keys) {
			setup_bob__role__keys(bob);
		} else {
			setup_bob__before__keys(bob);
		}
	}

	public static void setup_chuck__before__keys(Client chuck) {}

	public static void setup_chuck__role__keys(Client chuck) {
		chuck.setPrivateKey(789);
	}

	public static void setup_chuck(Client chuck) {
		if (FeatureSwitches.__SELECTED_FEATURE_keys) {
			setup_chuck__role__keys(chuck);
		} else {
			setup_chuck__before__keys(chuck);
		}
	}

	public static void setup_rjh__before__keys(Client rjh) {}

	public static void setup_rjh__role__keys(Client rjh) {
		rjh.setPrivateKey(456);
	}

	public static void setup_rjh(Client rjh) {
		if (FeatureSwitches.__SELECTED_FEATURE_keys) {
			setup_rjh__role__keys(rjh);
		} else {
			setup_rjh__before__keys(rjh);
		}
	}

}
