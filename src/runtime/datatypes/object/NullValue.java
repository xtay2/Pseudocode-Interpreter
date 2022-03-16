package runtime.datatypes.object;

import static runtime.datatypes.numerical.NumberValue.ZERO;

import building.types.specific.DataType;
import runtime.datatypes.BoolValue;
import runtime.datatypes.Value;
import runtime.datatypes.numerical.ConceptualNrValue;
import runtime.datatypes.numerical.IntValue;
import runtime.datatypes.numerical.NumberValue;
import runtime.datatypes.textual.TextValue;

public final class NullValue extends ObjectValue {

	public static final NullValue NULL = new NullValue();

	/** Text-Representation of this {@link ConceptualNrValue}. */
	public final String txt = "null";

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
			default -> false;
		};
	}

	@Override
	public boolean valueCompare(Value v) {
		return v == NULL;
	}

	@Override
	public Object raw() {
		return null;
	}
}
