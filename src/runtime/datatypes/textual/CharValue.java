package runtime.datatypes.textual;

import static building.types.specific.datatypes.SingleType.*;

import building.expressions.abstractions.interfaces.*;
import building.types.specific.datatypes.*;
import errorhandeling.*;
import runtime.datatypes.*;
import runtime.datatypes.numerical.*;

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
