package runtime.datatypes.textual;

import static building.types.specific.data.DataType.CHAR;

import java.math.BigInteger;

import building.types.specific.data.DataType;
import runtime.datatypes.Value;
import runtime.datatypes.numerical.IntValue;
import runtime.datatypes.numerical.NumberValue;
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
	public TextValue asText() {
		return new TextValue(String.valueOf(value));
	}

	@Override
	public boolean canCastTo(DataType type) {
		return switch (type) {
			case VAR, CHAR -> true; // Returns self
			case TEXT -> true; // Returns text-representation
			case NUMBER, INT -> true; // Returns ASCII-Representation
			case BOOL, DEF -> false;
			// Unimplemented
			case OBJECT -> false;
		};
	}

	@Override
	public NumberValue asNumber() {
		return asInt();
	}

	@Override
	public IntValue asInt() {
		return NumberValue.create(BigInteger.valueOf(value));
	}

	@Override
	public boolean valueCompare(Value v) throws UnexpectedTypeError {
		if (v instanceof CharValue ch)
			return value == ch.value;
		if (v instanceof TextValue txt) {
			String s = txt.raw();
			return s.length() == 1 && s.charAt(0) == value;
		}
		return false;
	}

	@Override
	public Character raw() {
		return value;
	}

}
