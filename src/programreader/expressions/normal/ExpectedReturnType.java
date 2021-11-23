package programreader.expressions.normal;

import programreader.expressions.special.Expression;
import programreader.program.ExpressionType;

public class ExpectedReturnType extends Expression {

	public ExpectedReturnType(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.EXPECTED_TYPE);
	}

}
