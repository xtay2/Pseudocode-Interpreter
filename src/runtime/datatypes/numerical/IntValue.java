package runtime.datatypes.numerical;

import static building.types.specific.DataType.INT;
import static runtime.datatypes.numerical.ConceptualNrValue.NAN;
import static runtime.datatypes.numerical.ConceptualNrValue.NEG_INF;
import static runtime.datatypes.numerical.ConceptualNrValue.POS_INF;

import java.math.BigInteger;

import building.types.specific.DataType;

/**
 * An Integer-Value with up to 100 digits.
 * 
 * @see DataType#INT
 */
public final class IntValue extends NumberValue {

	public final BigInteger value;

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
		if (equals(ZERO) || v == NAN || v.isInfinite())
			return NAN;
//		if (v instanceof DecimalValue d)
//			TODO
		if (v instanceof IntValue i)
			return create(value.mod(i.value));
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
//		if (v instanceof DecimalValue d)
//			TODO
//		if (v instanceof IntValue i)
//			TODO
		throw new AssertionError("Unimplemented Case.");
	}

	@Override
	public NumberValue root(NumberValue v) {
//		if (v == NAN || v.isInfinite())
//			TODO
		// 0^0 = NaN and x^0 = 1
		if (v.valueCompare(ZERO))
			return this.valueCompare(ZERO) ? NAN : ONE;
		if (v.valueCompare(ONE))
			return this;
//		if (v instanceof DecimalValue d)
//			TODO
//		if (v instanceof IntValue i)
//			TODO
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
