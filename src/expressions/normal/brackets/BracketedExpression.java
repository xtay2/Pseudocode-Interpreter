package expressions.normal.brackets;

import datatypes.Value;
import expressions.special.Expression;
import expressions.special.ValueHolder;
import helper.Output;
import parsing.program.ExpressionType;

public class BracketedExpression extends Expression implements ValueHolder {

	private final ValueHolder value;

	public BracketedExpression(int line, ValueHolder content) {
		super(line);
		setExpectedExpressions(ExpressionType.INFIX_OPERATOR);
		this.value = content;
	}

	@Override
	public Value getValue() {
		return value.getValue();
	}

	@Override
	public String toString() {
		return Output.DEBUG ? this.getClass().getSimpleName() : getValue().toString();
	}

}
