package helper;

import interpreter.VarManager;

public final class Output {

	public static final String LINE_BREAK = "\n" + "=".repeat(70) + "\n";
	public static final String UNDERLINE = "\n----------------------";

	public static final boolean DEBUG = false;

	public static void print(String text) {
		int identation = VarManager.countOfScopes();
		if (DEBUG)
			System.out.println("   ".repeat(identation > 0 ? identation - 1 : 0) + text);
	}

}
