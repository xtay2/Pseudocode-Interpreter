package expressions.normal;

import expressions.special.Expression;
import parser.program.ExpressionType;

public class Comma extends Expression {

	public Comma(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.EXPECTED_TYPE, ExpressionType.LITERAL, ExpressionType.NAME);
	}

}
