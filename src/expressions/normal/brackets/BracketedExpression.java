package expressions.normal.brackets;

import static types.SuperType.INFIX_OPERATOR;

import datatypes.Value;
import expressions.abstractions.Expression;
import expressions.abstractions.interfaces.MergedExpression;
import expressions.abstractions.interfaces.ValueHolder;
import types.SuperType;

public class BracketedExpression extends Expression implements ValueHolder, MergedExpression {

	private ValueHolder value;

	public BracketedExpression(int line) {
		super(line, SuperType.MERGED, INFIX_OPERATOR);
	}

	@Override
	public void merge(Expression... e) {
		if (e.length != 1)
			throw new AssertionError("A bracketed Expression only takes one Operation/Value.");
		value = (ValueHolder) e[0];
	}

	@Override
	public Value getValue() {
		return value.getValue();
	}
}
