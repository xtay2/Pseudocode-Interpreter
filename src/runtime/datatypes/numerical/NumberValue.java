package runtime.datatypes.numerical;

import static building.types.specific.data.DataType.INT;
import static building.types.specific.data.DataType.NUMBER;
import static runtime.datatypes.numerical.ConceptualNrValue.NAN;
import static runtime.datatypes.numerical.ConceptualNrValue.NEG_INF;
import static runtime.datatypes.numerical.ConceptualNrValue.POS_INF;

import java.math.BigDecimal;
import java.math.BigInteger;

import building.types.specific.data.DataType;
import runtime.datatypes.BoolValue;
import runtime.datatypes.TextValue;
import runtime.datatypes.Value;
import runtime.datatypes.array.ArrayValue;
import runtime.exceptions.CastingException;
import runtime.exceptions.UnexpectedTypeError;

public abstract class NumberValue extends Value {

	/** Total count of digits in an integer or num/denom in a fraction. */
	public static final int MAX_LENGTH = 100;

	// FINITE CONSTANTS
	public static final IntValue ONE = new IntValue(BigInteger.ONE);
	public static final IntValue ZERO = new IntValue(BigInteger.ZERO);
	public static final IntValue NEG_ONE = new IntValue(BigInteger.valueOf(-1));

	/** Creates a {@link IntValue} from a {@link BigInteger}. */
	public static IntValue create(BigInteger v) {
		return new IntValue(v);
	}

	/** Creates a {@link NumberValue} from a {@link BigDecimal}. */
	public static NumberValue create(BigDecimal val) {
		int decPoints = Math.max(0, val.stripTrailingZeros().scale());
		BigInteger denom = BigInteger.TEN.pow(decPoints);
		return create(val.multiply(new BigDecimal(denom)).toBigIntegerExact(), denom);
	}

	/**
	 * Creates a {@link NumberValue} from a fracional, possibly reduced rational.
	 */
	public static NumberValue create(BigInteger num, BigInteger denom) {
		// Reduction
		BigInteger gcd = num.gcd(denom);
		num = num.divide(gcd);
		denom = denom.divide(gcd);
		// Int-Checks
		if (num.equals(BigInteger.ZERO))
			return ZERO;
		if (denom.equals(BigInteger.ONE))
			return create(num);
		// Fractional result
		return new DecimalValue(num, denom);
	}

	/**
	 * Sets the type. {@link DecimalValue} has the type{@link DataType#NUMBER} and {@link IntValue} is a
	 * {@link DataType#INT}.
	 */
	protected NumberValue(DataType dataType) {
		super(dataType);
		if (dataType != INT && dataType != NUMBER)
			throw new AssertionError("DataType has to be INT or NUMBER.");
	}

	// PUBLIC FINAL

	/** Inverts the sign. */
	public final NumberValue negate() {
		return this.mult(NEG_ONE);
	}

	/** Returns the absolute value. */
	public final NumberValue abs() {
		if (isNegative())
			return negate();
		return this;
	}

	/** Checks if the value is positive. x >= 0 */
	public final boolean isPositive() {
		return isGreaterEq(ZERO) || this == POS_INF;
	}

	/** Checks if the value is negative. x < 0 */
	public final boolean isNegative() {
		return !isPositive();
	}

	/** Checks if the value is either POS_INF or NEG_INF. */
	public final boolean isInfinite() {
		return this == POS_INF || this == NEG_INF;
	}

	// COMPARISON

	/** this < n */
	public final boolean isSmallerThan(NumberValue v) {
		if (this == NAN || v == NAN)
			throw new ArithmeticException("The value of NaN cannot be compared.");
		if (this == POS_INF) // Pos inf is the greates val.
			return false;
		if (this == NEG_INF) // NegInf == NegInf
			return v != NEG_INF;
		if (this instanceof IntValue x && v instanceof IntValue y)
			return x.value.compareTo(y.value) < 0;
		if (this instanceof DecimalValue x && v instanceof DecimalValue y)
			return x.num.multiply(y.denom).compareTo(y.num.multiply(x.denom)) < 0;
		if (this instanceof DecimalValue && v instanceof IntValue i)
			return asInt().value.compareTo(i.value) < 0;
		if (this instanceof IntValue x && v instanceof DecimalValue y)
			return x.value.compareTo(y.asInt().value) <= 0;
		throw new AssertionError("Undefined Case for Number-Value Comparison");
	}

	/** a1 <= a2 */
	public final boolean isSmallerEq(NumberValue v) {
		return isSmallerThan(v) || valueCompare(v);
	}

	/** a1 > a2 */
	public final boolean isGreaterThan(NumberValue v) {
		return v.isSmallerThan(this);
	}

	/** a1 >= a2 */
	public final boolean isGreaterEq(NumberValue v) {
		return v.isSmallerEq(this);
	}

	// Casting

	/** Returns false if this value is NaN or true if it has a value. */
	@Override
	public final BoolValue asBool() {
		return BoolValue.valueOf(NAN != this);
	}

	@Override
	public final TextValue asText() {
		if (this == NAN)
			return new TextValue("NaN");
		if (this == POS_INF)
			return new TextValue("INF");
		if (this == NEG_INF)
			return new TextValue("-INF");
		if (this instanceof IntValue i)
			return new TextValue(i.value.toString());
		String s = ((DecimalValue) this).fractionToDecimal();
		if (s.matches("(\\d+)\\.((([1-9]+)(0+$))|(0+$))"))
			s = s.replaceAll("(\\.(0+)$|(0+)$)", "");
		return new TextValue(s);
	}

	/** Returns this. */
	@Override
	public final NumberValue asNumber() {
		return this;
	}

	/** Returns an {@link IntValue}-Representation. Floors, if necessary. */
	@Override
	public final IntValue asInt() {
		if (this instanceof IntValue i)
			return i;
		if (this instanceof DecimalValue dec)
			return create(dec.num.divide(dec.denom));
		throw new CastingException("Cannot cast " + raw().toString() + " to an IntValue.");
	}

	@Override
	public final ArrayValue asVarArray() {
		return asNumberArray();
	}

	@Override
	public final boolean canCastTo(DataType type) {
		return switch (type) {
			case VAR, NUMBER -> true; // Returns this
			case INT -> !(this instanceof ConceptualNrValue); // Only if this isn't NAN or Infinite.
			case BOOL -> true; // Returns false for NaN and true for everything else
			case TEXT -> true; // Text or CharArray-Representation
			// Not implemented
			case OBJECT -> false;
			// Not supported
			case DEF -> false;
		};
	}

	/** Value-comparison between this {@link NumberValue} and a {@link Value}. */
	@Override
	public final boolean valueCompare(Value v) {
		if (!(v instanceof NumberValue))
			throw new UnexpectedTypeError(v.type);
		return equals(v);
	}

	/**
	 * Value-comparison between this {@link NumberValue} and a {@link Value} or a {@link Number}.
	 */
	@Override
	public final boolean equals(Object obj) {
		// Fast case for (conceptual) constants.
		if (this == obj)
			return true;
		// Slow, shouldn't get used.
		if (obj instanceof Number nr)
			obj = create(new BigDecimal(nr.toString()));
		if (obj instanceof TextValue t)
			obj = t.asNumber();
		if (obj instanceof NumberValue n) {
			if (this instanceof IntValue x && obj instanceof IntValue y)
				return x.value.equals(y.value);
			// Turn to common denom then compare
			if (this instanceof DecimalValue x && obj instanceof DecimalValue y)
				return x.num.multiply(y.denom).equals(y.num.multiply(x.denom));
		}
		return false;
	}

	// Operations

	/** this + v */
	public abstract NumberValue add(NumberValue v);

	/** this - v */
	public abstract NumberValue sub(NumberValue v);

	/** this * v */
	public abstract NumberValue mult(NumberValue v);

	/** this / v */
	public abstract NumberValue div(NumberValue v);

	/** this % v */
	public abstract NumberValue mod(NumberValue v);

	/** this ^ v */
	public abstract NumberValue pow(NumberValue v);

	/** this root v */
	public abstract NumberValue root(NumberValue v);
}