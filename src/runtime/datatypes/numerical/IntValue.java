package runtime.datatypes.numerical;

import static building.types.specific.datatypes.ArrayType.INT_ARRAY;
import static building.types.specific.datatypes.ArrayType.TEXT_ARRAY;
import static building.types.specific.datatypes.ArrayType.VAR_ARRAY;
import static building.types.specific.datatypes.SingleType.INT;
import static runtime.datatypes.numerical.ConceptualNrValue.NAN;
import static runtime.datatypes.numerical.ConceptualNrValue.NEG_INF;
import static runtime.datatypes.numerical.ConceptualNrValue.POS_INF;

import java.math.BigDecimal;
import java.math.BigInteger;

import building.types.specific.datatypes.ArrayType;
import building.types.specific.datatypes.DataType;
import ch.obermuhlner.math.big.BigDecimalMath;
import runtime.datatypes.ArrayCastable;
import runtime.datatypes.array.ArrayValue;
import runtime.exceptions.CastingException;

/**
 * An Integer-Value with up to 100 digits.
 * 
 * @see DataType#INT
 */
public final class IntValue extends NumberValue implements ArrayCastable {

	public final BigInteger value;

	/** Produces a {@link IntValue} from a {@link Long}. */
	public IntValue(long value) {
		this(BigInteger.valueOf(value));
	}

	/** Produces a {@link IntValue} from a {@link BigInteger}. */
	protected IntValue(BigInteger value) {
		super(INT);
		this.value = value;
	}

	/** Returns true if this number is even. */
	public final boolean isEven() {
		return !isOdd();
	}

	/** Returns true if this number is odd. */
	public final boolean isOdd() {
		return value.testBit(0);
	}

	@Override
	public ArrayValue asArray(ArrayType at) {
		if (at.equals(VAR_ARRAY) || at.equals(INT_ARRAY) || at.equals(TEXT_ARRAY)) {
			final String line = value.toString();
			IntValue[] intArray = new IntValue[line.length()];
			for (int i = 0; i < intArray.length; i++)
				intArray[i] = new IntValue(Character.getNumericValue(line.charAt(i)));
			return new ArrayValue(at, false, intArray);
		}
		throw new CastingException("Cannot cast int to " + at + ".");
	}

	/** This should only get called in debugging scenarios. */
	@Override
	public String toString() {
		return value.toString();
	}

	@Override
	public NumberValue add(NumberValue v) {
		if (v == NAN || v.isInfinite())
			return v;
		if (v instanceof DecimalValue d)
			return create(value.multiply(d.denom).add(d.num), d.denom);
		if (v instanceof IntValue i)
			return create(value.add(i.value));
		throw new AssertionError("Unimplemented Case.");
	}

	@Override
	public NumberValue sub(NumberValue v) {
		return add(v.negate());
	}

	@Override
	public NumberValue mult(NumberValue v) {
		if (v == NAN)
			return v;
		if (v.equals(ZERO))
			return ZERO;
		if (v.isInfinite())
			return isPositive() ? v : v.negate();
		if (v instanceof DecimalValue d)
			return create(value.multiply(d.num), d.denom);
		if (v instanceof IntValue i)
			return create(value.multiply(i.value));
		throw new AssertionError("Unimplemented Case.");
	}

	@Override
	public NumberValue div(NumberValue v) {
		if (equals(ZERO))
			return this;
		if (v == NAN || v.isInfinite())
			return NAN;
		if (v instanceof DecimalValue d)
			return mult(create(d.denom, d.num));
		if (v instanceof IntValue i)
			return create(value, i.value);
		throw new AssertionError("Unimplemented Case.");
	}

	@Override
	public NumberValue mod(NumberValue v) {
		if (v.equals(ZERO) || v == NAN || v.isInfinite())
			return NAN;
		if (v instanceof DecimalValue d)
			return create(new BigDecimal(value).remainder(d.raw()));
		if (v instanceof IntValue i)
			return create(value.remainder(i.value));
		throw new AssertionError("Unimplemented Case.");
	}

	@Override
	public NumberValue pow(NumberValue v) {
		if (v == NAN || v == NEG_INF || (equals(ZERO) && v.equals(ZERO)))
			return NAN;
		if (v.equals(ZERO) || equals(ONE))
			return ONE;
		if (equals(ZERO))
			return ZERO;
		if (v == POS_INF)
			return isPositive() ? POS_INF : NAN;
		if (v instanceof DecimalValue d)
			create(BigDecimalMath.pow(new BigDecimal(value), d.raw(), PRECISION));
		if (v instanceof IntValue i)
			return create(BigDecimalMath.pow(new BigDecimal(value), new BigDecimal(i.value), PRECISION));
		throw new AssertionError("Unimplemented Case.");
	}

	/**
	 * Calculates the n'th root from v. (this = n)
	 */
	@Override
	public NumberValue root(NumberValue v) {
		if (v == NAN || v.isInfinite())
			return NAN;
		if (v.equals(ZERO))
			return ZERO;
		if (equals(ZERO))
			return v.equals(ONE) ? NAN : POS_INF;
		if (equals(ONE))
			return v;
		if (v instanceof DecimalValue d)
			return create(BigDecimalMath.root(d.raw(), new BigDecimal(value), PRECISION));
		if (v instanceof IntValue i)
			return create(BigDecimalMath.root(new BigDecimal(i.value), new BigDecimal(value), PRECISION));
		throw new AssertionError("Unimplemented Case.");
	}

	@Override
	public BigInteger raw() {
		return value;
	}

	/** Returns the faculty of this {@link IntValue}. */
	public IntValue fac() {
		BigInteger fac = BigInteger.ONE;
		for (long i = value.longValueExact(); i > 0; i--)
			fac = fac.multiply(BigInteger.valueOf(i));
		return create(fac);
	}
}
