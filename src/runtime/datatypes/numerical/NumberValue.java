package runtime.datatypes.numerical;

import static building.types.specific.datatypes.SingleType.INT;
import static building.types.specific.datatypes.SingleType.NUMBER;
import static runtime.datatypes.numerical.ConceptualNrValue.NAN;
import static runtime.datatypes.numerical.ConceptualNrValue.NEG_INF;
import static runtime.datatypes.numerical.ConceptualNrValue.POS_INF;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;

import building.types.specific.datatypes.DataType;
import building.types.specific.datatypes.SingleType;
import runtime.datatypes.Value;
import runtime.datatypes.textual.TextValue;
import runtime.exceptions.CastingException;

public abstract class NumberValue extends Value {

	/** Total count of digits in an integer or num/denom in a fraction. */
	public static final int MAX_LENGTH = 100;
	public static final MathContext PRECISION = new MathContext(MAX_LENGTH);

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

	/** Creates a {@link NumberValue} from a fracional, possibly reduced rational. */
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
		if (denom.equals(BigInteger.ZERO))
			return NAN;
		// Fractional result
		return new DecimalValue(num, denom);
	}

	/**
	 * Sets the type. {@link DecimalValue} has the type{@link DataType#NUMBER} and {@link IntValue} is a
	 * {@link DataType#INT}.
	 */
	protected NumberValue(DataType dataType) {
		super(dataType);
		assert dataType == INT || dataType == NUMBER : "DataType has to be INT or NUMBER.";
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
		if (this == v)
			return false;
		if (v == POS_INF || this == NEG_INF)
			return true;
		if (v == NEG_INF || this == POS_INF)
			return false;
		if (this instanceof IntValue x && v instanceof IntValue y)
			return x.value.compareTo(y.value) < 0;
		if (this instanceof DecimalValue x && v instanceof DecimalValue y)
			return x.num.multiply(y.denom).compareTo(y.num.multiply(x.denom)) < 0;
		if (this instanceof DecimalValue && v instanceof IntValue i)
			return asInt().value.compareTo(i.value) < 0;
		if (this instanceof IntValue x && v instanceof DecimalValue y)
			return x.value.compareTo(y.asInt().value) <= 0;
		throw new AssertionError("Undefined Case for Number-Value Comparison: " + this.raw() + " and " + v.raw());
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
	public final boolean canCastTo(SingleType type) {
		return switch (type) {
			case VAR, NUMBER -> true; // Returns this
			case INT -> !(this instanceof ConceptualNrValue); // Only if this isn't NAN or Infinite.
			case TEXT -> true; // Text or CharArray-Representation
			default -> false;
		};
	}

	/** Value-comparison between this {@link NumberValue} and a {@link Value}. */
	@Override
	public final boolean valueCompare(Value v) {
		if (v instanceof NumberValue)
			return equals(v);
		if (v instanceof TextValue txt)
			return txt.valueCompare(this);
		return false;
	}

	/** Value-comparison between this {@link NumberValue} and a {@link Value} or a {@link Number}. */
	@Override
	public final boolean equals(Object obj) {
		// Fast case for (conceptual) constants.
		if (this == obj)
			return true;
		if (obj instanceof Number nr) // This is important.
			obj = create(new BigDecimal(nr.toString()));
		if (obj instanceof NumberValue n) {
			if (this instanceof IntValue x && n instanceof IntValue y)
				return x.value.equals(y.value);
			// Turn to common denom then compare
			if (this instanceof DecimalValue x && n instanceof DecimalValue y)
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
