package datatypes;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;

import exceptions.runtime.CastingException;
import expressions.special.Type;

public class NumberValue extends Value {

	private final BigDecimal value;

	public NumberValue(double val) {
		value = new BigDecimal(val);
	}

	public NumberValue(BigInteger bigInteger) {
		this(new BigDecimal(bigInteger));
	}

	public NumberValue(BigDecimal val) {
		if (val.toString().length() > 100)
			throw new ArithmeticException("Number cannot have more than 100 digits.");
		value = val;
	}

	@Override
	public boolean canCastTo(Type type) {
		return switch (type) {
		case VAR -> true; // Gibt sich selbst zurück
		case NUMBER -> true; // Gibt sich selbst zurück
		case BOOL -> true; // Gibt false für 0 und true für alles andere wieder
		case NUMBER_ARRAY -> true; // Gibt die einzelnen Ziffern zurück
		case TEXT_ARRAY -> true; // Gibt text-repräsentation der Ziffern zurück
		case VAR_ARRAY -> true; // Gibt text-repräsentation der Ziffern zurück
		case TEXT -> true; // Gibt text-repräsentation zurück
		case BOOL_ARRAY -> value.toPlainString().matches("0|1"); // Geht nur für binärzahlen.
		};
	}

	@Override
	public ArrayValue asVarArray() throws CastingException {
		return new TextValue(value.toPlainString()).asVarArray();
	}

	@Override
	/** Only works for binary numbers. */
	public ArrayValue asBoolArray() throws CastingException {
		return new TextValue(value.toPlainString()).asBoolArray();
	}

	@Override
	public ArrayValue asTextArray() throws CastingException {
		return new TextValue(value.toPlainString()).asTextArray();
	}

	@Override
	public ArrayValue asNumberArray() throws CastingException {
		return new TextValue(value.toPlainString()).asNumberArray();
	}

	@Override
	public BoolValue asBool() throws CastingException {
		return new BoolValue(value.doubleValue() != 0);
	}

	@Override
	public NumberValue asNumber() throws CastingException {
		return this;
	}

	@Override
	public NumberValue asInt() {
		return new NumberValue(value.toBigInteger());
	}

	@Override
	public TextValue asText() throws CastingException {
		String s = value.toPlainString();
		if (s.matches("(\\d+)\\.((([1-9]+)(0+$))|(0+$))"))
			s = s.replaceAll("(\\.(0+)$|(0+)$)", "");
		return new TextValue(s);
	}

	@Override
	public Type getType() {
		return Type.NUMBER;
	}

	@Override
	public BoolValue eq(Value val) {
		return new BoolValue(val instanceof NumberValue t && t.value.equals(value));
	}

	@Override
	public BoolValue neq(Value val) {
		return new BoolValue(!(val instanceof NumberValue t && t.value.equals(value)));
	}

	/**
	 * Returns the raw long-integer value of this NumberValue.
	 * 
	 * Do not use this in an Operation!
	 */
	public long rawInt() {
		return value.longValueExact();
	}

	/**
	 * Returns the raw long-floating value of this NumberValue.
	 * 
	 * Do not use this in an Operation!
	 */
	public double rawFloat() {
		double a = value.doubleValue();
		return a;
	}

	// STATIC OPERATIONS

	/** Adds two NumberValues. */
	public static NumberValue add(NumberValue a1, NumberValue a2) {
		return new NumberValue(a1.value.add(a2.value));
	}

	/** Subtracts a NumberValue from another one. */
	public static NumberValue sub(NumberValue a1, NumberValue a2) {
		return new NumberValue(a1.value.subtract(a2.value));
	}

	/** Multiplies two NumberValues. */
	public static NumberValue mult(NumberValue a1, NumberValue a2) {
		return new NumberValue(a1.value.multiply(a2.value));
	}

	/** Divides a NumberValue by another one. */
	public static NumberValue div(NumberValue a1, NumberValue a2) {
		return new NumberValue(a1.value.divide(a2.value));
	}

	/** Performs integerdivision on two NumberValues. */
	public static NumberValue intDiv(NumberValue a1, NumberValue a2) {
		return div(a1, a2);
	}

	/** Returns the remainder of a Numbervalue divided by another one */
	public static NumberValue mod(NumberValue a1, NumberValue a2) {
		return new NumberValue(a1.value.remainder(a2.value));
	}

	/** Returns the power of a NumberValue. */
	public static NumberValue pow(NumberValue base, NumberValue exp) {
		return new NumberValue(base.value.pow(exp.value.intValueExact(), new MathContext(100)));
	}

	/** Returns a BoolValue true if a1 is smaller than a2 */
	public static BoolValue isSmallerThan(NumberValue a1, NumberValue a2) {
		return new BoolValue(a1.value.compareTo(a2.value) < 0);
	}

	/** Returns a BoolValue true if a1 is smaller or equal to a2 */
	public static BoolValue isSmallerEq(NumberValue a1, NumberValue a2) {
		return new BoolValue(a1.value.compareTo(a2.value) <= 0);
	}

}
