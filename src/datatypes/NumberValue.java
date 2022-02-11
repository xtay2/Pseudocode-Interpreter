package datatypes;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.util.HashMap;

import javax.naming.ldap.PagedResultsControl;

import ch.obermuhlner.math.big.BigDecimalMath;
import exceptions.runtime.CastingException;
import exceptions.runtime.UnexpectedTypeError;
import expressions.special.DataType;
import helper.Helper;
import helper.Output;
import interpreter.system.SystemFunctions;

/** An arbitrary Decimal Number with 100 digits of precision. */
public class NumberValue extends Value {

	// CONCEPTUAL CONSTANTS
	public static final NumberValue POS_INF = new NumberValue(State.POS_INF);
	public static final NumberValue NEG_INF = new NumberValue(State.NEG_INF);
	public static final NumberValue NAN = new NumberValue(State.NAN);

	// FINITE CONSTANTS
	public static final NumberValue ONE = new NumberValue(BigInteger.ONE, BigInteger.ONE);
	public static final NumberValue ZERO = new NumberValue(BigInteger.ZERO, BigInteger.ONE);
	public static final NumberValue NEG_ONE = new NumberValue(BigInteger.valueOf(-1), BigInteger.ONE);

	// MathContext
	public static final MathContext PRECISION = new MathContext(100);

	///////////////////////////////////////////////////////

	/** Values if state is finite, else null */

	/** NUMERATOR Shows the value for all non periodic numbers with length < 100. */
	private final BigInteger num;

	/** DENOMINATOR Is 1 for all numbers with lenght < 100. */
	private final BigInteger denom;

	///////////////////////////////////////////////////////

	/**
	 * Decides, if this number is finite, infinite or not a number. {@link State}.
	 */
	private final State state;

	/**
	 * Defines for each {@link NumberValue} if is finite, infinite or not a number.
	 */
	public enum State {
		FINITE(null),

		/**
		 * INF == INF INF > -INF INF is not comparable with NaN INF == true
		 * 
		 * If x is any finite number: INF > x x / INF = NaN INF / 0 = NaN INF / x = INF
		 * for x != 0 INF / -INF = NaN
		 */
		POS_INF("INF"),

		/**
		 * -INF == -INF -INF < INF -INF is not comparable with NaN -INF == true
		 * 
		 * If x is any finite number: -INF < x x / -INF = NaN -INF / 0 = NaN -INF / x =
		 * -INF for x != 0 -INF / INF = NaN
		 */
		NEG_INF("-INF"),

		/**
		 * NaN == NaN NaN == false NaN isn't comparable with anything except NaN. Every
		 * operation with NaN will be NaN.
		 */
		NAN("NaN");

		private final String name;

		State(String name) {
			this.name = name;
		}

		@Override
		public String toString() {
			if (name == null)
				throw new AssertionError("Finite types have a value.");
			return name;
		}
	}

	/**
	 * Creates a NumberValue from an integer.
	 */
	public static NumberValue create(BigInteger val) {
		return new NumberValue(val, BigInteger.ONE);
	}

	/**
	 * Creates a NumberValue from a decimal.
	 */
	public static NumberValue create(BigDecimal val) {
		int decPoints = Math.max(0, val.stripTrailingZeros().scale());
		BigInteger denom = BigInteger.TEN.pow(decPoints);
		return create(val.multiply(new BigDecimal(denom)).toBigIntegerExact(), denom);
	}

	/**
	 * Takes a fracional rational, reduces it, if possible, and then returns it as
	 * wrapped {@link NumberValue}.
	 * 
	 * @param num   numerator
	 * @param deNom denominator
	 * @return wrapping NumberValue
	 */
	public static NumberValue create(BigInteger num, BigInteger deNom) {
		if (num.equals(BigInteger.ZERO))
			return ZERO;
		BigInteger gcd = Helper.gcd(num, deNom);
		return new NumberValue(num.divide(gcd), deNom.divide(gcd));
	}

	/** Produces a rational Number. This fraction has to be reduced. */
	private NumberValue(BigInteger numerator, BigInteger denominator) {
		if (Helper.getDigitCount(numerator) > 100 || Helper.getDigitCount(denominator) > 100)
			throw new ArithmeticException("Numbers cannot extend 100 digits.");
		if (denominator.equals(BigInteger.ZERO))
			throw new ArithmeticException("Denominator cannot be zero, use NaN instead.");
		if (numerator.equals(BigInteger.ZERO) && !denominator.equals(BigInteger.ONE))
			throw new ArithmeticException("Nominator cannot be zero, use ZERO / call create() instead.");
		// -x / -y = x / y
		if (numerator.compareTo(BigInteger.ZERO) < 0 && denominator.compareTo(BigInteger.ZERO) < 0) {
			numerator = numerator.abs();
			denominator = denominator.abs();
		}
		// x / -y = -x / y
		else if (numerator.compareTo(BigInteger.ZERO) > 0 && denominator.compareTo(BigInteger.ZERO) < 0) {
			numerator = numerator.negate();
			denominator = denominator.abs();
		}
		this.num = numerator;
		this.denom = denominator;
		this.state = State.FINITE;
	}

	/**
	 * Constructor for conceptual constants:
	 * 
	 * {@link NumberValue#NAN}, {@link NumberValue#POS_INF},
	 * {@link NumberValue#NEG_INF}
	 */
	private NumberValue(State state) {
		if (state == State.FINITE)
			throw new AssertionError("Dont use this constructor for finite values.");
		this.num = null;
		this.denom = null;
		this.state = state;
	}

	// PRIVATE STATIC

	/** Returns true if only both of a1 or a2 have state s. */
	private static boolean both(NumberValue a1, NumberValue a2, State s) {
		return a1.state == s && a2.state == s;
	}

	/** Returns true if either of a1 or a2 (or) both have/has state s. */
	private static boolean either(NumberValue a1, NumberValue a2, State s) {
		return a1.state == s || a2.state == s;
	}

	/** POS_INF and NEG_INF or NEG_INF and POS_INF */
	private static boolean differentInfs(NumberValue a1, NumberValue a2) {
		return (a1 == POS_INF && a2 == NEG_INF) || (a1 == NEG_INF && a2 == POS_INF);
	}

	/**
	 * Turns an unchecked fraction into a String.
	 * 
	 * <pre>
	 * Example 1: n = 1, d = 2 Output: "0.5"
	 * 
	 * Example 2: n = 2, d = 1 Output: "2"
	 * 
	 * Example 3: n = 2, d = 3 Output: "0.(6)"
	 *  
	 * used in {@link #toString()} used in {@link #asText()}
	 * </pre>
	 * 
	 * @param n numerator
	 * @param d denominator
	 */
	private String fractionToDecimal(BigInteger n, BigInteger d) {
		StringBuilder sb = new StringBuilder();
		if (n.compareTo(BigInteger.ZERO) < 0 ^ d.compareTo(BigInteger.ZERO) < 0)
			sb.append('-');
		n = n.abs();
		d = d.abs();
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
	 * This gets used in {@link SystemFunctions}.
	 */
	public String asRational() {
		return state == State.FINITE ? num + "/" + denom : state.toString();
	}

	// PUBLIC STATIC

	/**
	 * Adds two NumberValues.
	 * 
	 * <pre>
	 * NaN + x = NaN 
	 * x + NaN = NaN
	 * 
	 * INF + -INF = NaN 
	 * -INF + INF = NaN
	 * 
	 * INF + x = INF 
	 * x + INF = INF
	 * 
	 * -INF + x = -INF 
	 * x + -INF = -INF
	 * </pre>
	 */
	public static NumberValue add(NumberValue a1, NumberValue a2) {
		if (both(a1, a2, State.FINITE)) {
			// (x / y) + (z / y) = (x + z) / y
			if (a1.denom.equals(a2.denom))
				return create(a1.num.add(a2.num), a1.denom);
			// (a / b) + (x / y) = (a * y) + (b * x)
			return create(a1.num.multiply(a2.denom).add(a1.denom.multiply(a2.num)), a1.denom.multiply(a2.denom));
		}
		if (either(a1, a2, State.NAN) || differentInfs(a1, a2))
			return NAN;
		if (either(a1, a2, State.POS_INF))
			return POS_INF;
		if (either(a1, a2, State.NEG_INF))
			return NEG_INF;
		throw new AssertionError("Unidentified Addition: " + a1 + " + " + a2);
	}

	/**
	 * Subtracts a NumberValue from another one. a - b = a + (-b) a - (-b) = a + b
	 */
	public static NumberValue sub(NumberValue a1, NumberValue a2) {
		return add(a1, signum(a2));
	}

	/**
	 * Multiplies two NumberValues.
	 * 
	 * <pre>
	 * NaN * x = NaN 
	 * x * NaN = NaN
	 * 
	 * x * 0 = 0
	 * 0 * x = 0
	 *  
	 * x * -INF = -INF 
	 * -INF * x = -INF
	 * 
	 * INF * x = INF 
	 * x * INF = INF
	 * </pre>
	 */
	public static NumberValue mult(NumberValue a1, NumberValue a2) {
		if (either(a1, a2, State.NAN) || differentInfs(a1, a2) || (a1.isInfinite() && a2.equals(ZERO))
				|| (a1.equals(ZERO) && a2.isInfinite()))
			return NAN;
		if (a1 == ZERO || a2 == ZERO)
			return ZERO;
		if ((a1.isNegative() && a2.isNegative() && either(a1, a2, State.NEG_INF))
				|| (a1.isPositive() && a2.isPositive() && either(a1, a2, State.POS_INF)))
			return POS_INF;
		if ((a1 == POS_INF && a2.isNegative()) || (a1.isNegative() && a2 == POS_INF) || (a1 == NEG_INF && a2.isPositive())
				|| (a1.isPositive() && a2 == NEG_INF))
			return NEG_INF;
		return create(a1.num.multiply(a2.num), a1.denom.multiply(a2.denom));
	}

	/**
	 * Divides a NumberValue by another one.
	 * 
	 * <pre>
	 * x / 0 = NaN
	 * 
	 * NaN / x = NaN 
	 * x / NaN = NaN
	 * 
	 * INF / INF = NaN 
	 * -INF / -INF = NaN 
	 * 
	 * INF / -INF = NaN 
	 * -INF / INF = NaN
	 * 
	 * x / INF = NaN
	 * x / -INF = NaN
	 * 
	 * INF / x = INF
	 * -INF / x = -INF
	 * </pre>
	 */
	public static NumberValue div(NumberValue a1, NumberValue a2) {
		if (a2.equals(ZERO) || either(a1, a2, State.NAN) || a2.isInfinite())
			return NAN;
		return mult(a1, new NumberValue(a2.denom, a2.num)); // create is called in mult

	}

	/**
	 * Returns the NumberValue of a base-value, raised to a power-value.
	 * 
	 * <pre>
	 * NaN ^ y = NaN 
	 * x ^ NaN = NaN
	 * 
	 * x ^ 1 = x
	 * 
	 * x ^ 0 = 1
	 * 1 ^ y = 1
	 * 
	 * x ^ -y =  1 / (x^y)
	 * 0 ^ +y = 0
	 * 
	 * x ^ y = x ^ y with finite nrs. 
	 * 
	 * (x > 1) ^ INF = INF
	 * (-1 < x < 1) ^ INF = 0
	 * (x <= -1) ^ INF = NaN
	 *  
	 * INF ^ +y = INF
	 * </pre>
	 */
	public static NumberValue pow(NumberValue base, NumberValue exp) {
		// NaN ^ y = NaN or x ^ NaN = NaN
		if (either(base, exp, State.NAN))
			return NAN;
		// x ^ 1 = x
		if (exp.equals(ONE))
			return base;
		// x ^ 0 = 1 or 1 ^ y = 1
		if (exp.equals(ZERO) || base.equals(ONE))
			return ONE;
		if (base == NEG_INF)
			return mult(POS_INF, pow(NEG_ONE, exp));
		// x ^ -y = 1 / (x^y)
		if (exp.isNegative())
			return div(ONE, pow(base, signum(exp)));
		// 0 ^ +y = 0
		if (base.equals(ZERO))
			return ZERO;
		if (both(base, exp, State.FINITE)) {
			if (exp.denom.equals(BigInteger.ONE))
				return create(BigDecimalMath.pow(new BigDecimal(base.num), new BigDecimal(exp.num), PRECISION));
			// @formatter:off 
			return root(
					create(exp.denom), 
					div(
						create(BigDecimalMath.pow(
								new BigDecimal(base.num), 
								new BigDecimal(exp.num), PRECISION)),
						create(BigDecimalMath.pow(
								new BigDecimal(base.denom), 
								new BigDecimal(exp.num), PRECISION))
						)
					);
			// @formatter:on
		}
		if (exp == POS_INF) {
			// (x > 1) ^ INF = INF
			if (base.isGreaterThan(ONE))
				return POS_INF;
			// (x < 1) ^ INF = NaN
			return NAN;
		}
		// the nth power of INF is always INF.
		if (base == POS_INF)
			return POS_INF;
		throw new AssertionError("Unidentified Power: " + base + "^" + exp);
	}

	/**
	 * Returns the nth root of a base-value.
	 * 
	 * @param deg is the nth root.
	 * @param rad
	 */
	public static NumberValue root(NumberValue deg, NumberValue rad) {
		// No Support for Complex Numbers (yet)
		if (either(deg, rad, State.NAN) || rad.isNegative() || deg.valueCompare(ZERO) || deg.isInfinite())
			return NAN;
		// the nth power of INF is always INF.
		if(rad == POS_INF)
			return POS_INF;
		// Root with Integer-Degree
		if (deg.denom.equals(BigInteger.ONE))
			// @formatter:off
			return div(
					create(BigDecimalMath.root(new BigDecimal(rad.num), new BigDecimal(deg.num), PRECISION)),
					create(BigDecimalMath.root(new BigDecimal(rad.denom), new BigDecimal(deg.num), PRECISION))
					);
			// @formatter:on
		return pow(rad, div(ONE, deg));
	}

	/**
	 * Performs a modulo operation on two NumberValues.
	 * 
	 * <pre>
	 * NaN % y = NaN 
	 * x % NaN = NaN
	 * 
	 * INF % x = NaN
	 * -INF % x = NaN
	 * 
	 * x % 0 = NaN
	 * 
	 * x % INF = x
	 * x % -INF = x
	 * 
	 * x % y  = x % y
	 * -x % y  = -(x % y)
	 * </pre>
	 */
	public static NumberValue mod(NumberValue a, NumberValue b) {
		// NaN % y = NaN or x % NaN = NaN or INF % x = NaN or -INF % x = NaN or x % 0 =
		// NaN
		if (either(a, b, State.NAN) || a.isInfinite() || b.equals(ZERO))
			return NAN;
		// x % INF = x or x % -INF = x
		if (b.isInfinite())
			return a;
		NumberValue c = div(a, b);
		return mult(sub(c, c.asInt()), b);
	}

	// COMPARISON

	/** a1 > a2 */
	public boolean isGreaterThan(NumberValue a) {
		return a.isSmallerEq(this);
	}

	/** a1 < a2 */
	public boolean isSmallerThan(NumberValue a) {
		if (both(this, a, State.FINITE))
			return num.multiply(a.denom).compareTo(a.num.multiply(denom)) < 0;
		if (either(this, a, State.NAN))
			throw new ArithmeticException("Cannot compare anything to NaN.");
		return (this != POS_INF && a == POS_INF) || (this == NEG_INF && a != NEG_INF);
	}

	/** a1 >= a2 */
	public boolean isGreaterEq(NumberValue a) {
		return a.isSmallerThan(this);
	}

	/** a1 <= a2 */
	public boolean isSmallerEq(NumberValue a) {
		return isSmallerThan(a) || valueCompare(a);
	}

	/** Checks if the value is positive. x > 0 */
	public boolean isPositive() {
		return isGreaterThan(ZERO) || this == POS_INF;
	}

	/** Checks if the value is negative. x < 0 */
	public boolean isNegative() {
		return isSmallerThan(ZERO) || this == NEG_INF;
	}

	/** Checks if the value is either POS_INF or NEG_INF. */
	public boolean isInfinite() {
		return this == POS_INF || this == NEG_INF;
	}

	// Casting

	/** Returns false if this value is NaN or true if it has a value. */
	@Override
	public BoolValue asBool() {
		return new BoolValue(!NAN.equals(this));
	}

	@Override
	/** Only works for binary numbers. */
	public ArrayValue asBoolArray() throws CastingException {
		throw new CastingException("A number cannot be turned into a bool-array.");
	}

	/** Performs integerdivision on num / denom, ie. floors this numbervalue. */
	@Override
	public NumberValue asInt() {
		if (state != State.FINITE)
			return this;
		return new NumberValue(num.divide(denom), BigInteger.ONE);
	}

	/** Do not call this if the datatype is known. */
	@Override
	public NumberValue asNumber() {
		return this;
	}

	@Override
	public ArrayValue asNumberArray() throws CastingException {
		throw new CastingException("A NumberValue cannot get casted to a NumberArray.");
	}

	@Override
	public TextValue asText() {
		if (this == NAN)
			return new TextValue("NaN");
		if (this == POS_INF)
			return new TextValue("INF");
		if (this == NEG_INF)
			return new TextValue("-INF");
		String s = fractionToDecimal(num, denom);
		if (s.matches("(\\d+)\\.((([1-9]+)(0+$))|(0+$))"))
			s = s.replaceAll("(\\.(0+)$|(0+)$)", "");
		return new TextValue(s);
	}

	@Override
	public ArrayValue asVarArray() throws CastingException {
		return asNumberArray();
	}

	@Override
	public boolean canCastTo(DataType type) {
		return switch (type) {
		case VAR -> true; // Gibt sich selbst zurück
		case NUMBER -> true; // Gibt sich selbst zurück
		case BOOL -> true; // Gibt false für NaN und true für alle anderen Zahlen wieder.
		case TEXT -> true; // Gibt text-repräsentation zurück
		case TEXT_ARRAY -> true; // Gibt text-repräsentation Zeichenweise zurück.
		case VAR_ARRAY, NUMBER_ARRAY, BOOL_ARRAY -> false;
		};
	}

	@Override
	public DataType getType() {
		return DataType.NUMBER;
	}

	/** Reverts the sign */
	static NumberValue signum(NumberValue n) {
		return switch (n.state) {
		case FINITE -> new NumberValue(n.num.negate(), n.denom);
		case NAN -> NAN;
		case NEG_INF -> POS_INF;
		case POS_INF -> NEG_INF;
		};
	}

	/**
	 * Returns the raw long-floating value of this NumberValue.
	 * 
	 * Do not use this in an Operation!
	 */
	@Deprecated // Should get a rework with infinity/nan
	public double rawFloat() {
		return num.doubleValue();
	}

	/**
	 * Returns the raw long-integer value of this NumberValue.
	 * 
	 * Do not use this in an Operation!
	 */
	@Deprecated // Should get a rework with infinity/nan
	public long rawInt() {
		return num.longValueExact();
	}

	/**
	 * Gets used to compare this NumberValue to primitive types. Should only be used
	 * in the static operations in this class.
	 * 
	 * For comparison-operators, use: {@link NumberValue#valueCompare(Value)}
	 */
	@Override
	public boolean equals(Object obj) {
		// ValueTypes
		if (obj instanceof Value n)
			return valueCompare(n);
		// Big Numbertypes
		if (obj instanceof BigInteger || obj instanceof BigDecimal)
			return valueCompare(create((BigDecimal) obj));
		// Primitive Types
		if (obj instanceof Number n)
			return valueCompare(create(new BigDecimal(n.doubleValue())));
		throw new AssertionError("Dont use equals on anything thats not a Value or Number-Class.");
	}

	/**
	 * Compares a NumberValues to another Value
	 * 
	 * <pre>
	 * With NumberValue 
	 * For finite numbers: x == y, INF == INF, -INF == -INF, NAN == NAN
	 * 
	 * With TextValue:
	 * Cast Text to Nr, then compare.
	 * 
	 * With BoolValue:
	 * Always NaN
	 * </pre>
	 */
	@Override
	public boolean valueCompare(Value v) throws UnexpectedTypeError {
		if (this == v)
			return true;
		if (v instanceof TextValue t)
			v = t.asNumber();
		if (v instanceof NumberValue n)
			return (both(this, n, State.FINITE) && num.equals(n.num) && denom.equals(n.denom));
		// If v instanceof ArrayVal / BoolVal / ObjVal
		return this == NAN;
	}

	/** This should only get called in debugging scenarios. */
	@Override
	public String toString() {
		if (Output.DEBUG)
			return this.getClass().getSimpleName();
		if (state == State.FINITE)
			return fractionToDecimal(num, denom);
		return state.toString();
	}

}
