package runtime.datatypes.textual;

import static building.types.specific.datatypes.SingleType.CHAR;

import building.types.specific.datatypes.DataType;
import runtime.datatypes.Value;
import runtime.datatypes.numerical.IntValue;
import runtime.exceptions.CastingException;
import runtime.exceptions.UnexpectedTypeError;

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
	public Value as(DataType t) throws CastingException {
		if (t.isArrayType())
			throw new CastingException(this, t);
		return switch (t.type) {
			case VAR, CHAR -> this;
			case NR, INT -> new IntValue(value);
			case TEXT -> new TextValue(String.valueOf(value));
			default -> throw new CastingException(this, t);
		};
	}

	@Override
	public boolean valueCompare(Value v) throws UnexpectedTypeError {
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
