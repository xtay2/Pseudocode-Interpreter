package runtime.datatypes;

import building.expressions.abstractions.interfaces.Castable;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.normal.containers.Variable;
import building.types.specific.datatypes.DataType;
import building.types.specific.datatypes.SingleType;
import runtime.datatypes.textual.TextValue;
import runtime.exceptions.CastingException;
import runtime.exceptions.ComparisonException;

/**
 * A wrapper {@link ValueHolder} that either contains a value or {@link #NULL}. This should get only
 * used inside of {@link Variable}s and parameters.
 */
public class MaybeValue implements ValueHolder, Castable {

	/** The one constant Null that gets referenced everytime there is "null" written in the code. */
	public static final Value NULL = new Value(SingleType.VAR) {

		public static final String txt = "null";

		@Override
		public boolean valueCompare(Value v) throws ComparisonException {
			return NULL == v;
		}

		@Override
		public Value as(DataType t) throws CastingException {
			if (t.isArrayType())
				throw new CastingException(this, t);
			return switch (t.type) {
				case VAR -> this;
				case TEXT -> new TextValue(txt);
				default -> throw new CastingException(this, t);
			};
		}

		@Override
		public Object raw() {
			return null;
		}

		@Override
		public String toString() {
			return txt;
		}
	};

	/** The wrapped value or {@link #NULL}. */
	private Value value;

	/** Construct a {@link MaybeValue} from another {@link ValueHolder}. */
	public MaybeValue(ValueHolder vh) {
		this.value = vh == null ? NULL : vh.getValue();
	}

	@Override
	public Value getValue() {
		return value;
	}

	// CASTING-----------------------------------------------

	@Override
	public Value as(DataType t) throws CastingException {
		return value == NULL ? NULL : value.as(t);
	}

	/** Changes the {@link DataType} of the wrapped value. Doesn't change {@link #NULL}. */
	public void castTo(DataType type) {
		if (value != NULL)
			value = value.as(type);
	}
}
