package runtime.datatypes;

import static building.types.specific.datatypes.SingleType.BOOL;
import static building.types.specific.datatypes.SingleType.NUMBER;
import static building.types.specific.datatypes.SingleType.TEXT;
import static building.types.specific.datatypes.SingleType.VAR;

import building.types.specific.datatypes.SingleType;
import runtime.datatypes.numerical.IntValue;
import runtime.datatypes.numerical.NumberValue;
import runtime.datatypes.textual.TextValue;

public class BoolValue extends Value {

	public final boolean value;

	public static final BoolValue TRUE = new BoolValue(true);
	public static final BoolValue FALSE = new BoolValue(false);

	/** Private constructor for the two constants. */
	private BoolValue(boolean val) {
		super(BOOL);
		value = val;
	}

	/** Returns the matching constant from a primitive boolean. */
	public static BoolValue valueOf(boolean val) {
		return val ? TRUE : FALSE;
	}

	@Override
	public BoolValue asBool() {
		return this;
	}

	@Override
	public NumberValue asNumber() {
		return asInt();
	}

	@Override
	public IntValue asInt() {
		return value ? NumberValue.ONE : NumberValue.ZERO;
	}

	@Override
	public TextValue asText() {
		return new TextValue(toString());
	}

	@Override
	public boolean canCastTo(SingleType type) {
		return switch (type) {
			case VAR, BOOL -> true; // Returns this
			case NUMBER, INT -> true; // Returns 0 or 1
			case TEXT -> true; // Text-Representation.
			default -> false;
		};
	}

	/**
	 * Inverts this boolean value.
	 * 
	 * If true, return false. If false, return true.
	 */
	public BoolValue not() {
		return valueOf(!value);
	}

	@Override
	public boolean valueCompare(Value v) {
		if (v instanceof BoolValue n)
			return n == TRUE;
		throw new AssertionError("Tried to compare " + this + " to " + v + ".");
	}

	@Override
	public Boolean raw() {
		return value;
	}

	@Override
	public String toString() {
		return String.valueOf(value);
	}
}
