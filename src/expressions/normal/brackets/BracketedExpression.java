package expressions.normal.brackets;

import static types.SuperType.MERGED;

import datatypes.Value;
import expressions.abstractions.Expression;
import expressions.abstractions.interfaces.ValueHolder;

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
