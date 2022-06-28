package building.expressions.normal.brackets;

import static building.types.abstractions.SpecificType.*;

import building.expressions.abstractions.*;
import building.expressions.abstractions.interfaces.*;
import runtime.datatypes.*;

/**
 * Wrapper {@link ValueHolder}.
 */
public class BracketedExpression extends Expression implements ValueHolder {
	
	private final ValueHolder value;
	
	public BracketedExpression(int lineID, ValueHolder val) {
		super(lineID, MERGED);
		value = val;
	}
	
	@Override
	public Value getValue() { return value.getValue(); }
}
