package expressions.special;

import java.util.regex.Pattern;

public final class Value {
	
	private Value() {
		//Is now a static Helper-Class
	}
	
	// Static: Is String a Castable
	
	public static boolean isValue(String arg) {
		return isNumber(arg) || isBoolean(arg) || isString(arg);
	}

	public static boolean isInteger(String value) {
		return Pattern.matches("\\d+", value);
	}
	
	public static boolean isNumber(String value) {
		return Pattern.matches("^(-?)(0|([1-9][0-9]*))(\\.[0-9]+)?$", value);
	}

	public static boolean isBoolean(String value) {
		if ("true".equals(value.toLowerCase()) || "false".equals(value.toLowerCase()))
			return true;
		return false;
	}

	public static boolean isString(String value) {
		return value.startsWith("\"") && value.endsWith("\"");
	}
}
