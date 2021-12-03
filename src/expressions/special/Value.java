package expressions.special;

import java.util.regex.Pattern;

import exceptions.CastingException;
import helper.Output;

public final class Value implements ValueHolder {

	private final Object value;
	private final Type type;

	public Value(String arg) {
		if (isBoolean(arg)) {
			this.value = "true".equals(arg);
			this.type = Type.BOOL;
		} else if (isInteger(arg)) {
			this.value = Integer.valueOf(arg);
			this.type = Type.NUMBER;
		} else if (isDouble(arg)) {
			this.value = Double.valueOf(arg);
			this.type = Type.NUMBER;
		} else if (isString(arg)) {
			this.value = arg.substring(1, arg.length() - 1);
			this.type = Type.TEXT;
		} else
			throw new IllegalArgumentException(arg + " is an illegal Value.");
	}

	public Value(Object val, Type type) {
		if ((!(val instanceof Boolean) || (type != Type.BOOL)) && (!(val instanceof Number) || (type != Type.NUMBER))
				&& (!(val instanceof String) || (type != Type.TEXT)))
			throw new IllegalArgumentException(val + " is an illegal Value for type " + type);
		this.type = type;
		this.value = val;
	}

	@Override
	public String toString() {
		return (Output.DEBUG ? type.getName() + "-Value: " : "") + value.toString();
	}

	public Type getType() {
		return type;
	}

	/**
	 * <pre>
	 * Converts a Variable to an Int.
	 *
	 * Int in String or Int to Int:
	 * "50" -> 50
	 *  50 	-> 50
	 *
	 * Double in String or Double to Int:
	 * "2.6" -> 2
	 *  2.6  -> 2
	 *
	 * Boolean in String, or Boolean to Int:
	 * "true" -> 1
	 *  true  -> 1
	 * "false" -> 0
	 *  false  -> 0
	 * </pre>
	 *
	 * @throws CastingException when trying to cast alphabetical text.
	 */
	public int asInt() {
		return asNr().intValue();
	}

	/**
	 * <pre>
	 * Converts a Variable to a Double.
	 *
	 * Int in String or Int to Double:
	 * "50" -> 50.0
	 *  50 	-> 50.0
	 *
	 * Double in String or Double to Double:
	 * "2.6" -> 2.6
	 *  2.6  -> 2.6
	 *
	 * Boolean in String, or Boolean to Double:
	 * "true" -> 1.0
	 *  true  -> 1.0
	 * "false" -> 0.0
	 *  false  -> 0.0
	 * </pre>
	 *
	 * @throws CastingException when trying to cast alphabetical text.
	 */
	public double asDouble() {
		return asNr().doubleValue();
	}

	/**
	 * <pre>
	 * Converts a Variable to a Boolean.
	 *
	 * String to Bool:
	 * "1" 	 -> true
	 * "1.1" -> true
	 * "true"-> true
	 *
	 * "0"	   -> false
	 * "0.0"   -> false
	 * "false" -> false
	 *
	 * Int to Bool
	 * 0 -> false
	 * every other number -> true
	 *
	 * Double to Bool:
	 * 0.0  -> false
	 * every other number -> true
	 * </pre>
	 *
	 * @throws CastingException when trying to cast alphabetical text.
	 */
	public boolean asBool() {
		switch (type) {
		case TEXT:
			String strVal = (String) value;
			if ("1".equals(strVal) || "1.0".equals(strVal) || "true".equals(strVal))
				return true;
			else if ("0".equals(strVal) || "0.0".equals(strVal) || "false".equals(strVal))
				return false;
			else
				throw new CastingException(
						"Cannot cast values other than \"true\"/\"false\", \"0\"/\"1\" and \"0.0\"/\"1.0\" from text to bool.");
		case NUMBER:
			if ((value instanceof Integer && (Integer) value == 0) || (value instanceof Double && (Double) value == 0.0))
				return false;
			return true;
		case BOOL:
			return (Boolean) value;
		default:
			throw new IllegalStateException("Value has to have one of three types.");
		}
	}

	/**
	 * <pre>
	 * Converts a Variable to a Number.
	 *
	 * Int in String or Int to Nr:
	 * "50" -> 50
	 *  50 	-> 50
	 *
	 * Double in String or Double to Nr:
	 * "2.6" -> 2.6
	 *  2.6  -> 2.6
	 *
	 * Boolean in String, or Boolean to Nr:
	 * "true" -> 1
	 *  true  -> 1
	 * "false" -> 0
	 *  false  -> 0
	 * </pre>
	 *
	 * @throws CastingException when trying to cast alphabetical text.
	 */
	public Number asNr() {
		switch (type) {
		case TEXT:
			String val = (String) value;
			if (isInteger(val))
				return Integer.valueOf(val);
			else if (isDouble(val)) {
				double nrVal = Double.valueOf(val);
				if (nrVal % 1 == 0.0)
					return (int) nrVal;
				return nrVal;
			} else if (isBoolean(val))
				return "true".equals(val) ? 1 : 0;
			else
				throw new CastingException("Cannot cast values other than numbers or boolean literals from text to number.");
		case BOOL:
			return (Boolean) value ? 1 : 0;
		case NUMBER:
			if (value instanceof Double) {
				double nrVal = (Double) value;
				if (nrVal % 1 == 0.0)
					return (int) nrVal;
				return nrVal;
			}
			if (value instanceof Integer)
				return (Integer) value;
		default:
			throw new IllegalStateException("Value has to have one of three types.");
		}
	}

	/**
	 * <pre>
	 * Converts a Variable to Text.
	 *
	 * Int to Text:
	 * 50 -> "50"
	 *
	 * Double to Text:
	 * 2.6 -> "2.6"
	 * 2.0 -> "2"
	 *
	 * Boolean to Text (Number-Representation for Circle-Casting.):
	 *  true  -> "1"
	 *  false -> "0"
	 * </pre>
	 */
	public String asText() {
		if (type == Type.BOOL)
			return (Boolean) value ? "1" : "0";
		if (type == Type.NUMBER)
			return asNr().toString();
		return value.toString();
	}

	public static boolean isValue(String arg) {
		return isInteger(arg) || isDouble(arg) || isBoolean(arg) || isString(arg);
	}

	private static boolean isInteger(String value) {
		return Pattern.matches("^(-?)(0|([1-9][0-9]*))$", value);
	}

	private static boolean isDouble(String value) {
		return Pattern.matches("^(-?)(0|([1-9][0-9]*))(\\.[0-9]+)?$", value);
	}

	private static boolean isBoolean(String value) {
		if ("true".equals(value.toLowerCase()) || "false".equals(value.toLowerCase()))
			return true;
		return false;
	}

	private static boolean isString(String value) {
		return value.startsWith("\"") && value.endsWith("\"");
	}

	/** Should only be called when used in an operation with a ValueHolder */
	@Override
	public Value getValue() {
		return this;
	}
}
