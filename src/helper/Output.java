package helper;

import modules.interpreter.VarManager;

public final class Output {

	public static final boolean DEBUG = false;
	public static final String LINE_BREAK = "\n" + "=".repeat(70) + "\n";

	public static final String UNDERLINE = "\n----------------------";

	public static void print(String text) {
		int identation = VarManager.countOfScopes();
		if (DEBUG)
			System.out.println("   ".repeat(identation > 0 ? identation - 1 : 0) + text);
	}

}
