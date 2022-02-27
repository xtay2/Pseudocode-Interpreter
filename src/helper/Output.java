package helper;

public final class Output {

	public static final boolean DEBUG = false;
	public static final String LINE_BREAK = "\n" + "=".repeat(70) + "\n";

	public static final String UNDERLINE = "\n----------------------";

	public static void print(String text) {
		if (DEBUG)
			System.out.println(text);
	}

}
