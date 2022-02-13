package datatypes;

import static parsing.program.ExpressionType.*;

import java.util.regex.Pattern;

import exceptions.runtime.CastingException;
import exceptions.runtime.UnexpectedTypeError;
import expressions.abstractions.Expression;
import expressions.abstractions.ValueHolder;
import expressions.special.DataType;
import parsing.program.ExpressionType;

public abstract class Value extends Expression implements ValueHolder {

	public Value() {
		super(-1, ExpressionType.LITERAL);
		setExpectedExpressions(COMMA, CLOSE_BRACKET, OPEN_SCOPE, INFIX_OPERATOR, TO, STEP, ARRAY_END,
				KEYWORD);
	}

	/**
	 * <pre>
	 * Compares the equality of values. (Commutative)
	 * 
	 * Comparable:
	 * -Values with identical types.
	 * -BoolValues with textual boolean literals.
	 * -Numbers with textual numbers.
	 * -Anything else with NaN
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

	// Static String-Checks

	/** This should get exclusivly used when casting from text to bool. */
	public static Boolean asBoolValue(String value) {
		if ("1".equals(value) || "1.0".equals(value) || "true".equals(value) || "yes".equals(value)
				|| "on".equals(value))
			return true;
		else if ("0".equals(value) || "0.0".equals(value) || "false".equals(value) || "no".equals(value)
				|| "off".equals(value))
			return false;
		else
			return null;
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
		return Pattern.matches("^(-?)(((0|(\\d*))(\\.\\d+)?)|(" + NumberValue.State.POS_INF.toString() + ")|("
				+ NumberValue.State.NAN.toString() + "))$", value);
	}

	public static boolean isString(String value) {
		return value.startsWith("\"") && value.endsWith("\"");
	}

	public static boolean isValue(String arg) {
		return isNumber(arg) || isBoolean(arg) || isString(arg);
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
		case VAR_ARRAY -> asVarArray();
		case TEXT_ARRAY -> asTextArray();
		case VAR -> getValue();
		default -> throw new UnexpectedTypeError("Unexpected type: " + t);
		};
	}

	public abstract BoolValue asBool() throws CastingException;

	public abstract ArrayValue asBoolArray() throws CastingException;

	public NumberValue asInt() {
		return asNumber().asInt();
	}

	/** Everything should have a text-representation. */
	public abstract TextValue asText();

	/** Everything can be casted to a number or NaN */
	public abstract NumberValue asNumber();

	public abstract ArrayValue asNumberArray() throws CastingException;

	/** Returns a characterwise textrepresentation. */
	public ArrayValue asTextArray() {
		return asText().asVarArray();
	}

	public abstract ArrayValue asVarArray() throws CastingException;

	/** Tells, if this Value can always be safely casted to the suggested type. */
	public abstract boolean canCastTo(DataType type);

	public abstract DataType getType();

	@Override
	public final Value getValue() {
		return this;
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
