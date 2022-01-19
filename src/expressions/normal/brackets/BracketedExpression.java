package expressions.normal.brackets;

import static parsing.program.ExpressionType.INFIX_OPERATOR;

import datatypes.Value;
import expressions.special.Expression;
import expressions.special.ValueHolder;
import helper.Output;

public class BracketedExpression extends Expression implements ValueHolder {

	private final ValueHolder value;

	public BracketedExpression(int line, ValueHolder content) {
		super(line);
		setExpectedExpressions(INFIX_OPERATOR);
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
