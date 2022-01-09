package expressions.normal;

import expressions.special.Expression;
import helper.Output;
import parsing.program.ExpressionType;

public class ExpectedReturnType extends Expression {

	public ExpectedReturnType(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.EXPECTED_TYPE);
	}

	@Override
	public String toString() {
		return Output.DEBUG ? this.getClass().getSimpleName() : "->";
	}
}
