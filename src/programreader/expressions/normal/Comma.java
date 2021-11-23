package programreader.expressions.normal;

import programreader.expressions.special.Expression;
import programreader.program.ExpressionType;

public class Comma extends Expression {
	
	public Comma(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.EXPECTED_TYPE, ExpressionType.LITERAL, ExpressionType.NAME);
	}

}
