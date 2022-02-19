package datatypes.numerical;

import static datatypes.numerical.ConceptualNrValue.NAN;

import java.math.BigInteger;
import java.math.MathContext;
import java.util.HashMap;

import helper.Helper;
import helper.Output;
import modules.interpreter.system.SystemFunctions;
import types.specific.DataType;

/** An arbitrary Decimal Number with 100 digits of precision. */
public final class DecimalValue extends NumberValue {

	// MathContext
	public static final MathContext PRECISION = new MathContext(MAX_LENGTH);

	/**
	 * The Numerator
	 * 
	 * <pre>
	 * Special case:
	 * 
	 * - Can never be {@link BigInteger#ZERO}, 
	 *   because then it would be {@link NumberValue#ZERO}.
	 * </pre>
	 */
	protected final BigInteger num;

	/**
	 * The Denominator
	 * 
	 * <pre>
	 * Special cases:
	 * 
	 * - Can never be {@link BigInteger#ONE}, 
	 *   because then it would be an {@link IntValue}.
	 *   
	 * - Can never be {@link BigInteger#ZERO}, 
	 *   because then it would be {@link ConceptualNrValue#NAN}.
	 * </pre>
	 */
	protected final BigInteger denom;

	/** Produces a rational Number. This fraction has to be already reduced. */
	protected DecimalValue(BigInteger numerator, BigInteger denominator) {
		super(DataType.NUMBER);
		// Assertions
		if (denominator.equals(BigInteger.ZERO))
			throw new AssertionError("Denominator cannot be zero, use NaN instead.");
		if (numerator.equals(BigInteger.ZERO))
			throw new AssertionError("Nominator cannot be zero, use ZERO instead.");
		if (denominator.equals(BigInteger.ONE))
			throw new AssertionError("Denominator cannot be one, use an IntValue instead.");
		// Length check
		if (Helper.getDigitCount(numerator) > MAX_LENGTH || Helper.getDigitCount(denominator) > MAX_LENGTH)
			throw new ArithmeticException("Numbers cannot extend 100 digits.");
		// Sign Check I: -x / -y = x / y
		if (numerator.compareTo(BigInteger.ZERO) < 0 && denominator.compareTo(BigInteger.ZERO) < 0) {
			numerator = numerator.abs();
			denominator = denominator.abs();
		}
		// Sign Check II: x / -y = -x / y
		else if (numerator.compareTo(BigInteger.ZERO) > 0 && denominator.compareTo(BigInteger.ZERO) < 0) {
			numerator = numerator.negate();
			denominator = denominator.abs();
		}
		this.num = numerator;
		this.denom = denominator;
	}

	/**
	 * Turns this into a String.
	 * 
	 * <pre>
	 * Example 1: n = 1, d = 2 Output: "0.5"
	 * 
	 * Example 2: n = 2, d = 3 Output: "0.(6)"
	 *  
	 * used in {@link #toString()} and {@link #asText()}
	 * </pre>
	 * 
	 * @param n numerator
	 * @param d denominator
	 */
	protected String fractionToDecimal() {
		StringBuilder sb = new StringBuilder();
		if (isNegative())
			sb.append('-');
		BigInteger n = num.abs(), d = denom.abs();
		BigInteger rem = n.remainder(d);
		sb.append(n.divide(d));
		if (rem.equals(BigInteger.ZERO))
			return sb.toString();
		sb.append('.');
		final HashMap<BigInteger, Integer> map = new HashMap<>();
		while (!rem.equals(BigInteger.ZERO)) {
			if (map.containsKey(rem)) {
				sb.insert(map.get(rem), "(");
				sb.append(")");
				break;
			}
			map.put(rem, sb.length());
			rem = rem.multiply(BigInteger.TEN);
			sb.append(rem.divide(d));
			if (sb.length() == PRECISION.getPrecision())
				break;
			rem = rem.remainder(d);
		}
		return sb.toString();
	}

	/**
	 * Returns this Number as a fractional text-representation.
	 * 
	 * Gets used in {@link SystemFunctions}.
	 */
	public String asRational() {
		return num + "/" + denom;
	}

	/** This should only get called in debugging scenarios. */
	@Override
	public String toString() {
		return Output.DEBUG ? getClass().getSimpleName() : fractionToDecimal();
	}

	// Operations

	@Override
	public NumberValue add(NumberValue v) {
		if (v instanceof DecimalValue d) {
			// Fast case: Equals denoms
			if (denom.equals(d.denom))
				return create(num.add(d.num), denom);
			// Common denom, then add
			return create(num.multiply(d.denom).add(d.num.multiply(denom)), denom.multiply(d.denom));
		}
		return v.add(this); // add() in IntVal or ConceptualNrVal
	}

	@Override
	public NumberValue sub(NumberValue v) {
		return add(v.negate());
	}

	@Override
	public NumberValue mult(NumberValue v) {
		if (v instanceof DecimalValue d)
			return create(num.multiply(d.num), denom.multiply(d.denom));
		return v.mult(this); // mult() in IntVal or ConceptualNrVal
	}

	@Override
	public NumberValue div(NumberValue v) {
		if (v.equals(ZERO) || v == NAN || v.isInfinite())
			return NAN;
		if (v instanceof DecimalValue d)
			return mult(create(d.denom, d.num));
		if (v instanceof IntValue i)
			return mult(create(BigInteger.ONE, i.value));
		throw new AssertionError("Unimplemented Case.");
	}

	@Override
	public NumberValue mod(NumberValue v) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public NumberValue pow(NumberValue v) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public NumberValue root(NumberValue v) {
		// TODO Auto-generated method stub
		return null;
	}
}
