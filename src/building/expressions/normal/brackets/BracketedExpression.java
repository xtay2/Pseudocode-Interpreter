package building.expressions.normal.brackets;

import static building.types.SuperType.MERGED;

import building.expressions.abstractions.Expression;
import building.expressions.abstractions.interfaces.ValueHolder;
import runtime.datatypes.Value;

/**
 * Wrapper {@link ValueHolder}.
 */
public class BracketedExpression extends Expression implements ValueHolder {

	private final ValueHolder value;

	public BracketedExpression(int lineID, ValueHolder val) {
		super(lineID, MERGED);
		this.value = val;
	}

	@Override
	public Value getValue() {
		return value.getValue();
	}
}
