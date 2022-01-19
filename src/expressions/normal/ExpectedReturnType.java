package expressions.normal;

import static parsing.program.ExpressionType.EXPECTED_TYPE;

import expressions.special.Expression;
import helper.Output;
public class ExpectedReturnType extends Expression {

	public ExpectedReturnType(int line) {
		super(line);
		setExpectedExpressions(EXPECTED_TYPE);
	}

	@Override
	public String toString() {
		return Output.DEBUG ? this.getClass().getSimpleName() : "->";
	}
}
