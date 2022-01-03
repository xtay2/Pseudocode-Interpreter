package datatypes;

import java.util.regex.Pattern;

import exceptions.runtime.CastingException;
import exceptions.runtime.UnexpectedTypeError;
import expressions.special.Type;
import expressions.special.ValueHolder;

public abstract class Value implements ValueHolder {

	public abstract ArrayValue asVarArray() throws CastingException;

	public abstract ArrayValue asBoolArray() throws CastingException;

	public abstract ArrayValue asTextArray() throws CastingException;

	public abstract ArrayValue asNumberArray() throws CastingException;

	public abstract BoolValue asBool() throws CastingException;

	public abstract NumberValue asNumber() throws CastingException;

	public abstract TextValue asText() throws CastingException;

	public abstract Type getType();

	public abstract BoolValue eq(Value val);

	public abstract BoolValue neq(Value value);

	public NumberValue asInt() {
		return asNumber().asInt();
	}

	/**
	 * Cast this value to the passed type.
	 * 
	 * If the type is allways the same, use the corresponding variant.
	 */
	public final Value as(Type t) {
		return switch (t) {
		case BOOL -> asBool();
		case BOOL_ARRAY -> asBoolArray();
		case NUMBER -> asNumber();
		case NUMBER_ARRAY -> asNumberArray();
		case TEXT -> asText();
		case TEXT_ARRAY -> asTextArray();
		case VAR_ARRAY -> asVarArray();
		case VAR -> getValue();
		default -> throw new UnexpectedTypeError("Unexpected type: " + t);
		};
	}

	@Override
	public Value getValue() {
		return this;
	}

	@Override
	public String toString() {
		return asText().rawString();
	}

	// Static String-Checks

	public static boolean isValue(String arg) {
		return isNumber(arg) || isBoolean(arg) || isString(arg);
	}

	public static boolean isInteger(String value) {
		return Pattern.matches("\\d+", value);
	}

	public static boolean isNumber(String value) {
		return Pattern.matches("^(-?)(0|(\\d*))(\\.\\d+)?$", value);
	}

	public static boolean isBoolean(String value) {
		if ("true".equals(value) || "false".equals(value))
			return true;
		return false;
	}

	public static boolean isString(String value) {
		return value.startsWith("\"") && value.endsWith("\"");
	}

}
