package costabs.messages;

import org.eclipse.osgi.util.NLS;

public class Messages extends NLS {
	private static final String BUNDLE_NAME = "costabs.messages.messages"; //$NON-NLS-1$
	public static String OptionsDialog2_available;
	public static String OptionsDialog2_error;
	public static String OptionsDialog2_2;

	public static String OptionsDialog2_4;
	public static String OptionsDialog2_5;
	public static String OptionsDialog2_6;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
