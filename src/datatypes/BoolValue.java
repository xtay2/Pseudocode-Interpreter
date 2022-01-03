package datatypes;

import exceptions.runtime.CastingException;
import expressions.special.Type;

public class BoolValue extends Value {

	private final boolean value;

	public BoolValue(boolean val) {
		value = val;
	}

	@Override
	public ArrayValue asVarArray() {
		throw new CastingException("A bool cannot be casted to an array.");
	}

	@Override
	public ArrayValue asBoolArray() throws CastingException {
		throw new CastingException("A bool cannot be casted to an array.");
	}

	@Override
	public ArrayValue asTextArray() throws CastingException {
		throw new CastingException("A bool cannot be casted to an array.");
	}

	@Override
	public ArrayValue asNumberArray() throws CastingException {
		throw new CastingException("A bool cannot be casted to an array.");
	}

	@Override
	public BoolValue asBool() {
		return this;
	}

	@Override
	public NumberValue asNumber() {
		return value ? new NumberValue(1) : new NumberValue(0);
	}

	@Override
	public TextValue asText() {
		return value ? new TextValue("true") : new TextValue("false");
	}

	@Override
	public Type getType() {
		return Type.BOOL;
	}

	@Override
	public BoolValue eq(Value val) {
		return new BoolValue(val instanceof BoolValue t && t.value == value);
	}

	@Override
	public BoolValue neq(Value val) {
		return new BoolValue(!(val instanceof BoolValue t && t.value == value));
	}

	/**
	 * Returns the raw boolean value of this BoolValue.
	 * 
	 * Do not use this in an Operation!
	 */
	public boolean rawBoolean() {
		return value;
	}

	public static Value and(BoolValue b1, BoolValue b2) {
		return new BoolValue(b1.value && b2.value);
	}
	
	public static Value nand(BoolValue b1, BoolValue b2) {
		return new BoolValue(!(b1.value && b2.value));
	}
	
	public static Value or(BoolValue b1, BoolValue b2) {
		return new BoolValue(b1.value || b2.value);
	}

	public static Value nor(BoolValue b1, BoolValue b2) {
		return new BoolValue(!(b1.value || b2.value));
	}

	public static Value xor(BoolValue b1, BoolValue b2) {
		return new BoolValue(b1.value ^ b2.value);
	}
}
