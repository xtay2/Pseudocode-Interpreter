package expressions.normal.array;

import expressions.special.Expression;
import parser.program.ExpressionType;

public class ArrayEnd extends Expression {
	public ArrayEnd(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.COMMA, ExpressionType.CLOSE_BRACKET, ExpressionType.DECLARATION, ExpressionType.NAME, ExpressionType.INFIX_OPERATOR);
	}
}
