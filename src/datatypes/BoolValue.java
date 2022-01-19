package datatypes;

import exceptions.runtime.CastingException;
import exceptions.runtime.UnexpectedTypeError;
import expressions.special.DataType;

public class BoolValue extends Value {

	private final boolean value;

	public BoolValue(boolean val) {
		value = val;
	}

	@Override
	public BoolValue asBool() {
		return this;
	}

	@Override
	public ArrayValue asBoolArray() throws CastingException {
		throw new CastingException("A bool cannot be casted to an array.");
	}

	@Override
	public NumberValue asNumber() {
		return value ? new NumberValue(1) : new NumberValue(0);
	}

	@Override
	public ArrayValue asNumberArray() throws CastingException {
		throw new CastingException("A bool cannot be casted to an array.");
	}

	@Override
	public TextValue asText() {
		return value ? new TextValue("true") : new TextValue("false");
	}

	@Override
	public ArrayValue asTextArray() throws CastingException {
		throw new CastingException("A bool cannot be casted to an array.");
	}

	@Override
	public ArrayValue asVarArray() {
		throw new CastingException("A bool cannot be casted to an array.");
	}

	@Override
	public boolean canCastTo(DataType type) {
		return switch (type) {
		case VAR -> true; // Gibt sich selbst zurück
		case BOOL -> true; // Gibt sich selbst zurück
		case NUMBER -> true; // Gibt 0 oder 1 zurück
		case TEXT -> true; // Gibt text-repräsentation zurück
		case NUMBER_ARRAY -> false;
		case BOOL_ARRAY -> false;
		case TEXT_ARRAY -> false;
		case VAR_ARRAY -> false;
		};
	}

	@Override
	public DataType getType() {
		return DataType.BOOL;
	}

	
	/**
	 * Inverts this boolean value.
	 * 
	 * If true, return false. If false, return true.
	 */
	public BoolValue not() {
		return new BoolValue(!value);
	}
	
	/**
	 * Returns the raw boolean value of this BoolValue.
	 * 
	 * Do not use this in an Operation!
	 */
	public boolean raw() {
		return value;
	}

	@Override
	public boolean valueCompare(Value v) throws UnexpectedTypeError {
		if (v instanceof BoolValue n)
			return value == n.value;
		throw new UnexpectedTypeError("Tried to compare " + this + " to " + v + ".");
	}
}
