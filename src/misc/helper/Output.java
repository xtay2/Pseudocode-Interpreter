package misc.helper;

public final class Output {

	public static final boolean DEBUG = false;

	public static final String UNDERLINE = "\n----------------------";

	public static void print(Object text) {
		if (DEBUG)
			System.out.println(text);
	}

}
