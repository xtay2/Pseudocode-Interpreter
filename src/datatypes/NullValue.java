package datatypes;

import static datatypes.numerical.NumberValue.ZERO;

import datatypes.numerical.IntValue;
import datatypes.numerical.NumberValue;
import types.specific.data.DataType;

public final class NullValue extends ObjectValue {

	public static final NullValue NULL = new NullValue();

	/** Gets only called by {@link NullValue#NULL} once. */
	private NullValue() {
		if (NULL != null)
			throw new AssertionError("Null cannot be instantiated twice.");
	}

	@Override
	public BoolValue asBool() {
		return BoolValue.FALSE;
	}

	@Override
	public TextValue asText() {
		return new TextValue("null");
	}

	@Override
	public NumberValue asNumber() {
		return asInt();
	}

	@Override
	public IntValue asInt() {
		return ZERO;
	}

	@Override
	public boolean canCastTo(DataType type) {
		return switch (type) {
		case VAR, OBJECT -> true; // Always returns NULL
		case BOOL -> true; // Always returns false
		case NUMBER, INT -> true; // Always returns ZERO
		case TEXT -> true; // Always returns "null"
		};
	}

	@Override
	public boolean valueCompare(Value v) {
		return v == NULL;
	}
}
