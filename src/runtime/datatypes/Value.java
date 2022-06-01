package runtime.datatypes;

import static runtime.datatypes.MaybeValue.NULL;

import java.math.BigDecimal;
import java.math.BigInteger;

import building.expressions.abstractions.interfaces.ValueHolder;
import building.types.specific.datatypes.DataType;
import building.types.specific.datatypes.SingleType;
import runtime.datatypes.array.ArrayValue;
import runtime.datatypes.numerical.ConceptualNrValue;
import runtime.datatypes.numerical.DecimalValue;
import runtime.datatypes.numerical.IntValue;
import runtime.datatypes.textual.CharValue;
import runtime.datatypes.textual.TextValue;

public abstract class Value implements ValueHolder {

	public final SingleType dataType;

	/** Creates a new {@link Value}. */
	public Value(SingleType dataType) {
		this.dataType = dataType;
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
		return BoolValue.valueOf(a.valueCompare(b));
	}

	/**
	 * Should get implemented by all Classes that inherit this class (Value).
	 *
	 * @param v is the value its checked against.
	 */
	public abstract boolean valueCompare(Value v);

	@Override
	public final Value getValue() {
		return this;
	}

	/**
	 * Tells, if this {@link Value} matches the given {@link DataType}.
	 *
	 * <pre>
	 * Rules:
	 * -Every {@link SingleType} matches {@link SingleType#VAR}
	 * -The {@link DataType} can allow or forbid {@link MaybeValue#NULL}
	 * -Every constrained {@link SingleType} can be cast to it's matching supertype.
	 * -An {@link ArrayValue} only matches if the ranges of a proposed {@link DataType} are equal or bigger that its actual lengths.
	 * </pre>
	 *
	 */
	public final boolean matches(DataType type) {
		if (this == NULL)
			return type.allowsNull && type.getDims() == 0;
		if (this instanceof ArrayValue av)
			return av.allowsLosslessCastingTo(type);
		return dataType.is(type.type) && type.getDims() == 0;
	}

	/**
	 * Returns the {@link Object} that this {@link Value} holds.
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
