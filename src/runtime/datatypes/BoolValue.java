package runtime.datatypes;

import static building.types.specific.datatypes.SingleType.*;

import building.types.specific.datatypes.*;
import errorhandeling.*;
import runtime.datatypes.numerical.*;
import runtime.datatypes.textual.*;

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
	public Value as(DataType t) throws NonExpressionException {
		if (t.isArrayType())
			throw new NonExpressionException("Casting", "Cannot cast " + this + " to " + t + ".");
		return switch (t.type) {
			case VAR, BOOL -> this;
			case NR, INT -> value ? NumberValue.ONE : NumberValue.ZERO;
			case TEXT -> new TextValue(toString());
			default -> throw new NonExpressionException("Casting", "Cannot cast " + this + " to " + t + ".");
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
