package runtime.datatypes;

import java.math.BigDecimal;
import java.math.BigInteger;

import building.expressions.abstractions.Expression;
import building.expressions.abstractions.interfaces.Castable;
import building.types.specific.datatypes.ArrayType;
import building.types.specific.datatypes.DataType;
import building.types.specific.datatypes.SingleType;
import runtime.datatypes.array.ArrayValue;
import runtime.datatypes.numerical.ConceptualNrValue;
import runtime.datatypes.numerical.DecimalValue;
import runtime.datatypes.numerical.IntValue;
import runtime.datatypes.textual.CharValue;
import runtime.datatypes.textual.TextValue;
import runtime.exceptions.ComparisonException;
import runtime.exceptions.UnexpectedTypeError;

public abstract class Value extends Expression implements Castable {

	/** Creates a new {@link Value}. The lineID is -1 because this has no position. */
	public Value(DataType dataType) {
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
	 * @throws UnexpectedTypeError if the types aren't comparable. */
	public static final BoolValue eq(Value a, Value b) {
		return a.type == b.type ? BoolValue.valueOf(a.valueCompare(b)) : BoolValue.FALSE;
	}

	/** Tells, if this Value can always be safely casted to the suggested {@link DataType}.
	 * 
	 * Default: True for charwise-, text-, numeric-, representation. */
	@Override
	public boolean canCastTo(SingleType type) {
		return type == SingleType.TEXT || type == SingleType.NUMBER;
	}

	/** Tells, if this Value can always be safely casted to the suggested {@link ArrayType}.
	 * 
	 * Default: false for anything except {@link ArrayValue}s. */
	@Override
	public boolean canCastTo(ArrayType type) {
		return false;
	}

	/** Should get implemented by all Classes that inherit this class (Value).
	 * 
	 * @param v is the value its checked against.
	 * @throws UnexpectedTypeError if isn't an instance of the same class this method gets executed
	 * on. */
	public abstract boolean valueCompare(Value v) throws ComparisonException;

	@Override
	public Value getValue() {
		return this;
	}

	@Override
	public DataType getType() {
		return (DataType) type;
	}

	/** Returns the {@link Object} that this {@link Value} holds.
	 * 
	 * <pre>
	 * The return-type is:
	 * -{@link BigDecimal} for {@link DecimalValue}.
	 * -{@link BigInteger} for {@link IntValue}.
	 * -{@link Double} for {@link ConceptualNrValue}.
	 * 
	 * -{@link String} for {@link TextValue}.
	 * -{@link Character} for {@link CharValue}.
	 * 
	 * -{@link Boolean} for {@link BoolValue}.
	 * 
	 * -{@link Object} (null) for {@link NullValue}.
	 * 
	 * -{@link Value[]} for {@link ArrayValue}.
	 * </pre>
	 */
	public abstract Object raw();
}
