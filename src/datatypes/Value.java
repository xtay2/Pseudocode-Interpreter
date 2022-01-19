package datatypes;

import java.util.regex.Pattern;

import exceptions.runtime.CastingException;
import exceptions.runtime.UnexpectedTypeError;
import expressions.special.DataType;
import expressions.special.Expression;
import expressions.special.ValueHolder;

import static parsing.program.ExpressionType.*;

public abstract class Value extends Expression implements ValueHolder {

	/** This should get exclusivly used when casting from text to bool. */
	public static Boolean asBoolValue(String value) {
		if ("1".equals(value) || "1.0".equals(value) || "true".equals(value) || "yes".equals(value) || "on".equals(value))
			return true;
		else if ("0".equals(value) || "0.0".equals(value) || "false".equals(value) || "no".equals(value) || "off".equals(value))
			return false;
		else
			return null;
	}

	/**
	 * <pre>
	 * Compares the equality of values. (Commutative)
	 * 
	 * Comparable:
	 * -Values with identical types.
	 * -BoolValues with textual boolean literals.
	 * -Numbers with textual numbers.
	 * 
	 * Not Comparable:
	 * -Arrays with Numbers or Booleans
	 * -Numbers with textual booleans/anything
	 * </pre>
	 * 
	 * @throws UnexpectedTypeError if the types aren't comparable.
	 */
	public static final BoolValue eq(Value a, Value b) throws UnexpectedTypeError {
		if (a.getType() == b.getType())
			return new BoolValue(a.valueCompare(b));
		throw new UnexpectedTypeError("Tried to compare Values of type " + a.getType() + " and " + b.getType() + ".");
	}

	/**
	 * This should get exclusivly used when checking if a String matches the literal
	 * words "true" or "false".
	 */
	public static boolean isBoolean(String value) {
		if ("true".equals(value) || "false".equals(value))
			return true;
		return false;
	}

	public static boolean isInteger(String value) {
		return Pattern.matches("\\d+", value);
	}

	public static boolean isNumber(String value) {
		return Pattern.matches("^(-?)(0|(\\d*))(\\.\\d+)?$", value);
	}

	public static boolean isString(String value) {
		return value.startsWith("\"") && value.endsWith("\"");
	}

	public static boolean isValue(String arg) {
		return isNumber(arg) || isBoolean(arg) || isString(arg);
	}

	public Value() {
		super(-1);
		setExpectedExpressions(COMMA, CLOSE_BRACKET, OPEN_BLOCK, INFIX_OPERATOR, LOOP_CONNECTOR, ARRAY_END, DEFINITE_LINEBREAK);
	}

	/**
	 * Cast this value to the passed type.
	 * 
	 * If the type is allways the same, use the corresponding variant.
	 */
	public final Value as(DataType t) {
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

	public abstract BoolValue asBool() throws CastingException;

	public abstract ArrayValue asBoolArray() throws CastingException;

	public NumberValue asInt() {
		return asNumber().asInt();
	}

	public abstract NumberValue asNumber() throws CastingException;

	public abstract ArrayValue asNumberArray() throws CastingException;

	public abstract TextValue asText() throws CastingException;

	public abstract ArrayValue asTextArray() throws CastingException;

	// Static String-Checks

	public abstract ArrayValue asVarArray() throws CastingException;

	/** Tells, if this Value can always be safely casted to the suggested type. */
	public abstract boolean canCastTo(DataType type);

	public abstract DataType getType();

	@Override
	public Value getValue() {
		return this;
	}

	@Override
	public String toString() {
		return asText().rawString();
	}

	/**
	 * Should get implemented by all Classes that inherit this class (Value).
	 * 
	 * @param v is the value its checked against.
	 * @throws UnexpectedTypeError if isn't an instance of the same class this
	 *                             method gets executed on.
	 */
	public abstract boolean valueCompare(Value v) throws UnexpectedTypeError;

}
