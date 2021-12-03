package expressions.normal;

import expressions.special.Expression;
import parser.program.ExpressionType;

public class LoopConnector extends Expression {

	public LoopConnector(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.LITERAL, ExpressionType.NAME);
	}

}
