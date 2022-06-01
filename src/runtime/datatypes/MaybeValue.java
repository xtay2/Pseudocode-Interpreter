package runtime.datatypes;

import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.normal.containers.Variable;
import building.types.specific.datatypes.DataType;
import building.types.specific.datatypes.SingleType;
import errorhandeling.NonExpressionException;
import runtime.datatypes.textual.TextValue;

/**
 * A wrapper {@link ValueHolder} that either contains a value or {@link #NULL}. This should get only
 * used inside of {@link Variable}s and parameters.
 */
public class MaybeValue implements ValueHolder {

	/** The one constant Null that gets referenced everytime there is "null" written in the code. */
	public static final Value NULL = new Value(SingleType.VAR) {

		public static final String txt = "null";

		@Override
		public boolean valueCompare(Value v) {
			return NULL == v;
		}

		@Override
		public Value as(DataType t) throws NonExpressionException {
			if (t.isArrayType())
				throw new NonExpressionException("Casting", "Cannot cast " + this + " to " + t);
			return switch (t.type) {
				case VAR -> this;
				case TEXT -> new TextValue(txt);
				default -> throw new NonExpressionException("Casting", "Cannot cast " + this + " to " + t);
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
	public Value as(DataType t) throws NonExpressionException {
		return value == NULL ? NULL : value.as(t);
	}

	/**
	 * Changes the {@link DataType} of the wrapped value. Doesn't change {@link #NULL}.
	 *
	 * @throws NonExpressionException -> Casting
	 */
	public void castTo(DataType type) throws NonExpressionException {
		if (value != NULL)
			value = value.as(type);
	}
}
