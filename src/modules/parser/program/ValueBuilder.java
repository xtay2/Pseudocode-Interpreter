package modules.parser.program;

import datatypes.TextValue;

public final class ValueBuilder {

	/**
	 * Replace escaped characters with the real ascii values.
	 */
	public static TextValue escapeText(String arg) {
		for (int i = 0; i < arg.length() - 1; i++) {
			if (arg.charAt(i) == '\\') {
				char c = switch (arg.charAt(i + 1)) {
					case 't' -> '\t';
					case 'r' -> '\r';
					case 'n' -> '\n';
					case 'f' -> '\f';
					case '\\' -> '\\';
					case '"' -> '"';
					default -> throw new IllegalArgumentException("Unexpected value: " + arg.charAt(i + 1));
				};
				arg = arg.substring(0, i) + c + arg.substring(i + 2);
			}
		}
		return new TextValue(arg);
	}
}