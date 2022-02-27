package datatypes;

import static datatypes.numerical.ConceptualNrValue.NAN;
import static datatypes.numerical.ConceptualNrValue.NEG_INF;
import static datatypes.numerical.ConceptualNrValue.POS_INF;
import static types.SuperType.BUILDER_TYPE;
import static types.SuperType.INFIX_OPERATOR;
import static types.SuperType.KEYWORD_TYPE;
import static types.specific.ExpressionType.OPEN_SCOPE;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.regex.Pattern;

import datatypes.numerical.ConceptualNrValue;
import datatypes.numerical.DecimalValue;
import datatypes.numerical.IntValue;
import datatypes.numerical.NumberValue;
import datatypes.object.NullValue;
import exceptions.runtime.CastingException;
import exceptions.runtime.UnexpectedTypeError;
import expressions.abstractions.Expression;
import expressions.abstractions.interfaces.ValueHolder;
import modules.parser.program.ValueBuilder;
import types.specific.ExpressionType;
import types.specific.data.ArrayType;
import types.specific.data.DataType;
import types.specific.data.ExpectedType;

public abstract class Value extends Expression implements ValueHolder {

	/** Creates a new {@link Value}. The lineID is -1 because this has no position. */
	public Value(ExpectedType dataType) {
		super(dataType, OPEN_SCOPE, INFIX_OPERATOR, KEYWORD_TYPE, BUILDER_TYPE);
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
	public static final BoolValue eq(Value a, Value b) {
		if (a.type == b.type)
			return BoolValue.valueOf(a.valueCompare(b));
		throw new AssertionError("Tried to compare Values of type " + a.type + " and " + b.type + ".");
	}

	// Static String-Checks

	/**
	 * This should get exclusivly used when checking if a String matches the literal words "true" or
	 * "false".
	 */
	private static boolean isBoolean(String value) {
		if ("true".equals(value) || "false".equals(value))
			return true;
		return false;
	}

	/**
	 * Checks if this String is an acceptable Number.
	 * 
	 * (Digits with optional minus, decimal point and period Brackets or any {@link ConceptualNrValue})
	 */
	public static boolean isNumber(String value) {
		return Pattern.matches("^(-?)(((0|(\\d*))(\\.\\d+)?)|(" + POS_INF.txt + ")|(" + NAN.txt + "))$", value);
	}

	/** Checks if this String starts and ends with the symbol " */
	private static boolean isString(String value) {
		return value.startsWith("\"") && value.endsWith("\"");
	}

	/**
	 * Builds a {@link Value} from a {@link String}.
	 * 
	 * Called in {@link ExpressionType#create(String, int)}
	 * 
	 * Returns null if the {@link String} is not a literal value.
	 */
	public static Value stringToLiteral(String arg) {
		// Is Single Value
		if (Value.isBoolean(arg))
			return BoolValue.valueOf("true".equals(arg));
		if (Value.isNumber(arg)) {
			if (POS_INF.txt.equals(arg))
				return POS_INF;
			if (NEG_INF.txt.equals(arg))
				return NEG_INF;
			if (NAN.txt.equals(arg))
				return NAN;
			return DecimalValue.create(new BigDecimal(arg));
		}
		if (Value.isString(arg))
			return ValueBuilder.escapeText(arg.substring(1, arg.length() - 1));
		return null;
	}

	/**
	 * Cast this value to the passed type.
	 * 
	 * If the type is allways the same, use the corresponding variant.
	 */
	public final Value as(ExpectedType t) {
		return switch (t) {
			case DataType d:
				yield switch (d) {
					case VAR -> getValue();
					case BOOL -> asBool();
					case NUMBER -> asNumber();
					case INT -> asInt();
					case TEXT -> asText();
					case OBJECT -> throw new UnsupportedOperationException("Unsupported case: " + d);
				};
			case ArrayType a:
				yield switch (a) {
					case VAR_ARRAY -> asVarArray();
					case BOOL_ARRAY -> asBoolArray();
					case INT_ARRAY -> asIntArray();
					case NUMBER_ARRAY -> asNumberArray();
					case TEXT_ARRAY -> asTextArray();
					case OBJECT_ARRAY -> throw new UnsupportedOperationException("Unsupported case: " + a);
				};
			default:
				throw new IllegalArgumentException("Unexpected value: " + t);
		};
	}

	/** Everything should have a text-representation. */
	public abstract TextValue asText();

	/** Everything can be casted to a number or NaN */
	public NumberValue asNumber() {
		return NAN;
	}

	/** Returns a characterwise textrepresentation for default. */
	public ArrayValue asVarArray() {
		return asText().asVarArray();
	}

	/** Returns a characterwise textrepresentation for default. */
	public ArrayValue asTextArray() {
		return asText().asTextArray();
	}

	// Optional Casting

	public BoolValue asBool() throws CastingException {
		throw new CastingException("A " + type + " cannot be casted to a BoolValue.");
	}

	public IntValue asInt() throws CastingException {
		throw new CastingException("A " + type + " cannot be casted to a IntValue.");
	}

	public ArrayValue asBoolArray() throws CastingException {
		throw new CastingException("A " + type + " cannot be casted to a BoolArray.");
	}

	public ArrayValue asNumberArray() throws CastingException {
		throw new CastingException("A " + type + " cannot be casted to a NumberArray.");
	}

	public ArrayValue asIntArray() throws CastingException {
		throw new CastingException("A " + type + " cannot be casted to a IntArray.");
	}

	/** Tells, if this Value can always be safely casted to the suggested {@link DataType}. */
	public abstract boolean canCastTo(DataType type);

	/**
	 * Tells, if this Value can always be safely casted to the suggested {@link ArrayType}.
	 * 
	 * Default: Only true for charwise-text-representation, ie {@link ArrayType#TEXT_ARRAY}.
	 */
	public boolean canCastTo(ArrayType type) {
		return type == ArrayType.TEXT_ARRAY;
	}

	/**
	 * Should get implemented by all Classes that inherit this class (Value).
	 * 
	 * @param v is the value its checked against.
	 * @throws UnexpectedTypeError if isn't an instance of the same class this method gets executed on.
	 */
	public abstract boolean valueCompare(Value v) throws UnexpectedTypeError;

	@Override
	public final Value getValue() {
		return this;
	}

	/**
	 * Returns the Java-Object that holds this value.
	 * 
	 * <pre>
	 * The return-type is: 
	 * -{@link BigInteger} for {@link IntValue}.
	 * -{@link Double} for {@link ConceptualNrValue}.
	 * -{@link BigDecimal} for {@link DecimalValue}.
	 * 
	 * -{@link String} for {@link TextValue}.
	 * -{@link Boolean} for {@link BoolValue}.
	 * -{@link Object} (null) for {@link NullValue}.
	 * 
	 * -{@link Value[]} for {@link ArrayValue}.
	 * </pre>
	 */
	public abstract Object raw();
}
