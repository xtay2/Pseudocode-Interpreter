package runtime.datatypes.textual;

import static building.types.specific.datatypes.SingleType.CHAR;

import building.expressions.abstractions.interfaces.ValueHolder;
import building.types.specific.datatypes.DataType;
import errorhandeling.NonExpressionException;
import runtime.datatypes.Value;
import runtime.datatypes.numerical.IntValue;

/**
 * A {@link CharValue} is just a single {@link Character}. It can allways be casted to a text and
 * back.
 */
public class CharValue extends Value {

	private final char value;

	public CharValue(char c) {
		super(CHAR);
		value = c;
	}

	@Override
	public Value as(DataType t) throws NonExpressionException {
		if (t.isArrayType())
			ValueHolder.throwCastingExc(this, t);
		return switch (t.type) {
			case VAR, CHAR -> this;
			case NR, INT -> new IntValue(value);
			case TEXT -> new TextValue(String.valueOf(value));
			default -> ValueHolder.throwCastingExc(this, t);
		};
	}

	@Override
	public boolean valueCompare(Value v) {
		if (v instanceof CharValue ch)
			return value == ch.value;
		if (v instanceof TextValue txt)
			return txt.valueCompare(this);
		return false;
	}

	@Override
	public Character raw() {
		return value;
	}

	@Override
	public String toString() {
		return "'" + value + "'";
	}

}
