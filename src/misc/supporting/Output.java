package misc.supporting;

public final class Output {

	public static boolean debugMode = false;

	public static final String UNDERLINE = "\n----------------------";

	public static void print(Object text) {
		if (debugMode)
			System.out.println(text);
	}

}
