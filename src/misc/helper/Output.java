package misc.helper;

public final class Output {

	public static final boolean DEBUG = true;

	public static final String UNDERLINE = "\n----------------------";

	public static void print(String text) {
		if (DEBUG)
			System.out.println(text);
	}

}
