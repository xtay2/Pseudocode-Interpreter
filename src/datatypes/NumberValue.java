package datatypes;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;

import ch.obermuhlner.math.big.BigDecimalMath;
import exceptions.runtime.CastingException;
import exceptions.runtime.UnexpectedTypeError;
import expressions.special.DataType;
import helper.Output;

/** An arbitrary Decimal Number with 100 digits of precision. */
public class NumberValue extends Value {

	/** NumberValues can have at most 100 digits of precision. */
	private final static MathContext PRECISION = new MathContext(100);

	/** Value if state is finite, else null */
	private final BigDecimal value;

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

	/** Create a finite Number from a big decimal value. */
	public NumberValue(BigDecimal val) {
		if (val.precision() > PRECISION.getPrecision())
			throw new ArithmeticException("Number cannot have more than 100 digits.");
		value = val;
		state = State.FINITE;
	}

	/** Create a finite Number from a big integer value. */
	public NumberValue(BigInteger bigInteger) {
		this(new BigDecimal(bigInteger));
	}

	/** Create a Number from a double value. Used for 0, 1, -1. */
	public NumberValue(double val) {
		this(new BigDecimal(val));
		if (Double.isNaN(val) || Double.isInfinite(val))
			throw new AssertionError("Use the constants for Infinity or NaN instead.");
	}

	// CONSTANTS

	public static final NumberValue POS_INF = new NumberValue(State.POS_INF);
	public static final NumberValue NEG_INF = new NumberValue(State.NEG_INF);
	public static final NumberValue NAN = new NumberValue(State.NAN);

	public static final NumberValue ZERO = new NumberValue(0);
	public static final NumberValue ONE = new NumberValue(1);

	/**
	 * Constructor for constants:
	 * 
	 * {@link NumberValue#NAN}, {@link NumberValue#POS_INF},
	 * {@link NumberValue#NEG_INF}
	 */
	private NumberValue(State state) {
		if (state == State.FINITE)
			throw new AssertionError("Dont use this constructor for finite values.");
		value = null;
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
		if (both(a1, a2, State.FINITE))
			return new NumberValue(a1.value.add(a2.value));
		if (either(a1, a2, State.NAN) || differentInfs(a1, a2))
			return NAN;
		if (either(a1, a2, State.POS_INF))
			return POS_INF;
		if (either(a1, a2, State.NEG_INF))
			return NEG_INF;
		throw new AssertionError("Unidentified Addition: " + a1 + " + " + a2);
	}

	/**
	 * Subtracts a NumberValue from another one.
	 * 
	 * <pre>
	 * NaN - x = NaN 
	 * x - NaN = NaN
	 * 
	 * INF - INF = NaN
	 * -INF - -INF = NaN
	 * 
	 * INF - x = INF 
	 * x - INF = -INF
	 * 
	 * -INF - x = -INF 
	 * x - -INF = INF
	 * </pre>
	 */
	public static NumberValue sub(NumberValue a1, NumberValue a2) {
		if (both(a1, a2, State.FINITE))
			return new NumberValue(a1.value.subtract(a2.value));
		if (either(a1, a2, State.NAN) || both(a1, a2, State.POS_INF) || both(a1, a2, State.NEG_INF))
			return NAN;
		if (a1 == POS_INF || a2 == NEG_INF)
			return POS_INF;
		if (a1 == NEG_INF || a2 == POS_INF)
			return NEG_INF;
		throw new AssertionError("Unidentified Subtraction: " + a1 + " + " + a2);
	}

	/**
	 * Multiplies two NumberValues.
	 * 
	 * <pre>
	 * NaN * x = NaN 
	 * x * NaN = NaN
	 * 
	 * INF * 0 = NaN
	 * 0 * INF = NaN
	 *  
	 * -INF * 0 = NaN
	 * 0 * -INF = NaN
	 * 
	 * x * -INF = -INF 
	 * -INF * x = -INF
	 * 
	 * INF * x = INF 
	 * x * INF = INF
	 * </pre>
	 */
	public static NumberValue mult(NumberValue a1, NumberValue a2) {
		if (both(a1, a2, State.FINITE))
			return new NumberValue(a1.value.multiply(a2.value));
		if (either(a1, a2, State.NAN) || (a1.isInfinity() && a2.equals(ZERO)) || (a1.equals(ZERO) && a2.isInfinity()))
			return NAN;
		if (either(a1, a2, State.NEG_INF))
			return NEG_INF;
		if (either(a1, a2, State.POS_INF))
			return POS_INF;
		throw new AssertionError("Unidentified Multiplication: " + a1 + " * " + a2);
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
		if (a2.equals(ZERO) || either(a1, a2, State.NAN) || a2.isInfinity())
			return NAN;
		if (both(a1, a2, State.FINITE))
			return new NumberValue(a1.value.divide(a2.value));
		if (a1.isInfinity())
			return a1;
		throw new AssertionError("Unidentified Division: " + a1 + " + " + a2);
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
			return new NumberValue(base.value);
		// x ^ 0 = 1 or 1 ^ y = 1
		if (exp.equals(ZERO) || base.equals(ONE))
			return ONE;
		// x ^ -y = 1 / (x^y)
		if (exp.isNegative())
			return div(ONE, pow(base, signum(exp)));
		// 0 ^ +y = 0
		if (base.equals(ZERO))
			return ZERO;
		// x ^ y = x ^ y with finite nrs.
		if (both(base, exp, State.FINITE))
			return new NumberValue(BigDecimalMath.pow(base.value, exp.value, PRECISION));
		if (exp == POS_INF) {
			// (x > 1) ^ INF = INF
			if (base.isGreaterThan(ONE))
				return POS_INF;
			// (-1 < x < 1) ^ INF = 0
			if (base.isGreaterThan(new NumberValue(-1)) && base.isSmallerThan(ONE))
				return ZERO;
			// (x <= -1) ^ INF = NaN
			return NAN;
		}
		// INF ^ +y = INF
		if (base == POS_INF)
			return ZERO;
		throw new AssertionError("Unidentified Power: " + base + "^" + exp);
	}

	/**
	 * Returns the nth root of a base-value.
	 * 
	 * @param root is the nth root.
	 * @param base
	 */
	public static NumberValue root(NumberValue root, NumberValue base) {
		return pow(base, div(ONE, root));
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
	public static NumberValue mod(NumberValue a1, NumberValue a2) {
		// NaN % y = NaN or x % NaN = NaN or INF % x = NaN or -INF % x = NaN or x % 0 =
		// NaN
		if (either(a1, a2, State.NAN) || a1.isInfinity() || a2.equals(ZERO))
			return NAN;
		// x % INF = x or x % -INF = x
		if (a2.isInfinity())
			return a1;
		// x % y = x % y or x % -y = x % y or -x % y = -(x % y) or -x % -y = -(x % y)
		return new NumberValue(a1.value.abs().remainder(a2.value));
	}

	// COMPARISON

	/** a1 > a2 */
	public boolean isGreaterThan(NumberValue a) {
		return a.isSmallerEq(this);
	}

	/** a1 < a2 */
	public boolean isSmallerThan(NumberValue a) {
		if (both(this, a, State.FINITE))
			return value.compareTo(a.value) < 0;
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
		if (isInfinity() && a.isInfinity())
			return isSmallerThan(a) || this.valueCompare(a);
		return isSmallerThan(a) || this.value.compareTo(a.value) == 0;
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
	public boolean isInfinity() {
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
		return new TextValue(value.toPlainString()).asBoolArray();
	}

	@Override
	public NumberValue asInt() {
		if (state != State.FINITE)
			return this;
		return new NumberValue(value.toBigInteger());
	}

	/** Do not call this if the datatype is known. */
	@Override
	public NumberValue asNumber() {
		return this;
	}

	@Override
	public ArrayValue asNumberArray() throws CastingException {
		if (state != State.FINITE)
			throw new CastingException(this + " cannot get casted to a NumberArray.");
		return new TextValue(value.toPlainString()).asNumberArray();
	}

	@Override
	public TextValue asText() {
		if (this == NAN)
			return new TextValue("NaN");
		if (this == POS_INF)
			return new TextValue("INF");
		if (this == NEG_INF)
			return new TextValue("-INF");
		String s = value.toPlainString();
		if (s.matches("(\\d+)\\.((([1-9]+)(0+$))|(0+$))"))
			s = s.replaceAll("(\\.(0+)$|(0+)$)", "");
		return new TextValue(s);
	}

	@Override
	public ArrayValue asTextArray() {
		return asText().asTextArray();
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
		case TEXT_ARRAY -> true; // Gibt text-repräsentation der Ziffern zurück
		case TEXT -> true; // Gibt text-repräsentation zurück
		case NUMBER_ARRAY -> state == State.FINITE; // Gibt die einzelnen Ziffern zurück
		case VAR_ARRAY -> state == State.FINITE; // Gibt die einzelnen Ziffern zurück
		case BOOL_ARRAY -> state == State.FINITE && value.toPlainString().matches("[0|1]"); // Geht nur für binärzahlen.
		};
	}

	@Override
	public DataType getType() {
		return DataType.NUMBER;
	}

	/** Reverts the sign */
	private static NumberValue signum(NumberValue n) {
		return switch (n.state) {
		case FINITE -> mult(n, new NumberValue(-1));
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
		return value.doubleValue();
	}

	/**
	 * Returns the raw long-integer value of this NumberValue.
	 * 
	 * Do not use this in an Operation!
	 */
	@Deprecated // Should get a rework with infinity/nan
	public long rawInt() {
		return value.longValueExact();
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
			return valueCompare(new NumberValue((BigDecimal) obj));
		// Primitive Types
		if (obj instanceof Number n)
			return valueCompare(new NumberValue(n.doubleValue()));
		throw new AssertionError("Dont use equals on anything thats not a Value or Number-Class.");
	}

	/**
	 * Compares two NumberValues. If anything else gets passed, an
	 * {@link UnexpectedTypeError} gets thrown.
	 * 
	 * For finite numbers: INF == INF, -INF == -INF, NAN == NAN
	 */
	@Override
	public boolean valueCompare(Value v) throws UnexpectedTypeError {
		if (v instanceof NumberValue n)
			return (both(this, n, State.FINITE) && value.equals(n.value)) || this == v;
		throw new UnexpectedTypeError("Tried to compare " + this + " to " + v + ".");
	}

	@Override
	public String toString() {
		return Output.DEBUG ? this.getClass().getSimpleName() : (state == State.FINITE ? value.toString() : state.toString());
	}

}
