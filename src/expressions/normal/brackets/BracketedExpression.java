package expressions.normal.brackets;

import static parsing.program.ExpressionType.INFIX_OPERATOR;

import datatypes.Value;
import expressions.abstractions.Expression;
import expressions.abstractions.MergedExpression;
import expressions.abstractions.ValueHolder;
import parsing.program.ExpressionType;

public class BracketedExpression extends Expression implements ValueHolder, MergedExpression {

	private ValueHolder value;

	public BracketedExpression(int line) {
		super(line, ExpressionType.MERGED);
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
