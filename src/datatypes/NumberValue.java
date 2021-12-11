package datatypes;

import java.math.BigDecimal;
import java.math.BigInteger;

import exceptions.CastingException;
import expressions.special.Type;

public class NumberValue extends Castable {

	private final BigDecimal value;

	public NumberValue(double val) {
		value = new BigDecimal(val);
	}

	public NumberValue(BigInteger bigInteger) {
		value = new BigDecimal(bigInteger);
	}

	public NumberValue(BigDecimal val) {
		value = val;
	}

	@Override
	public ArrayValue asVarArray() throws CastingException {
		return new TextValue(value.toPlainString()).asVarArray();
	}

	@Override
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
		return new BoolValue(value.doubleValue() != 0.0);
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
		return new TextValue(value.toPlainString());
	}

	@Override
	public Type getType() {
		return Type.NUMBER;
	}

	@Override
	public BoolValue eq(Castable val) {
		return new BoolValue(val instanceof NumberValue t && t.value.equals(value));
	}

	@Override
	public BoolValue neq(Castable val) {
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
	public static Castable div(NumberValue a1, NumberValue a2) {
		return new NumberValue(a1.value.divide(a2.value));
	}

	/** Returns the remainder of a Numbervalue divided by another one */
	public static Castable mod(NumberValue a1, NumberValue a2) {
		return new NumberValue(a1.value.remainder(a2.value));
	}

	public static BoolValue isSmallerThan(NumberValue a1, NumberValue a2) {
		return new BoolValue(a1.value.compareTo(a2.value) < 0);
	}

	public static BoolValue isSmallerEq(NumberValue a1, NumberValue a2) {
		return new BoolValue(a1.value.compareTo(a2.value) <= 0);
	}
	
	@Override
	public String toString() {
		return value.toPlainString();
	}
}
