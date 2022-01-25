package expressions.normal;

import static parsing.program.ExpressionType.EXPECTED_TYPE;

public class ExpectedReturnType extends Expression {

	public ExpectedReturnType(int line) {
		super(line);
		setExpectedExpressions(EXPECTED_TYPE);
	}
}
