package expressions.normal;

import expressions.special.Expression;
import parser.program.ExpressionType;

public class ExpectedReturnType extends Expression {

	public ExpectedReturnType(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.EXPECTED_TYPE);
	}

}
