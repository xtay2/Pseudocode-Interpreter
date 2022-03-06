package runtime.datatypes;

import static runtime.datatypes.numerical.ConceptualNrValue.NAN;

import java.math.BigDecimal;
import java.math.BigInteger;

import building.expressions.abstractions.Expression;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.types.specific.data.ArrayType;
import building.types.specific.data.DataType;
import building.types.specific.data.ExpectedType;
import runtime.datatypes.array.ArrayValue;
import runtime.datatypes.numerical.ConceptualNrValue;
import runtime.datatypes.numerical.DecimalValue;
import runtime.datatypes.numerical.IntValue;
import runtime.datatypes.numerical.NumberValue;
import runtime.datatypes.object.NullValue;
import runtime.exceptions.CastingException;
import runtime.exceptions.UnexpectedTypeError;

public abstract class Value extends Expression implements ValueHolder {

	/** Creates a new {@link Value}. The lineID is -1 because this has no position. */
	public Value(ExpectedType dataType) {
		super(dataType);
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
					case DEF -> asDef();
					case OBJECT -> throw new UnsupportedOperationException("Unsupported case: " + d);
				};
			case ArrayType a:
				yield switch (a) {
					case VAR_ARRAY -> asVarArray();
					case BOOL_ARRAY -> asBoolArray();
					case INT_ARRAY -> asIntArray();
					case NUMBER_ARRAY -> asNumberArray();
					case TEXT_ARRAY -> asTextArray();
					case DEF_ARRAY -> asDefArray();
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

	public DefValue asDef() throws CastingException {
		throw new CastingException("A " + type + " cannot be casted to a DefValue.");
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

	public ArrayValue asDefArray() throws CastingException {
		throw new CastingException("A " + type + " cannot be casted to a DefArray.");
	}

	/** Tells, if this Value can always be safely casted to the suggested {@link ExpectedType}. */
	public final boolean canCastTo(ExpectedType t) {
		if (t instanceof DataType dt)
			return canCastTo(dt);
		if (t instanceof ArrayType at)
			return canCastTo(at);
		throw new AssertionError("Expected Type " + t + " is neither a data-, or array-type.");
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
