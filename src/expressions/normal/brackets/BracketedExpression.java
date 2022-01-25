package expressions.normal.brackets;

import static parsing.program.ExpressionType.INFIX_OPERATOR;

import datatypes.Value;
import expressions.normal.Expression;
import expressions.special.MergedExpression;
import expressions.special.ValueHolder;

public class BracketedExpression extends Expression implements ValueHolder, MergedExpression {

	private ValueHolder value;

	public BracketedExpression(int line) {
		super(line);
		setExpectedExpressions(INFIX_OPERATOR);
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
