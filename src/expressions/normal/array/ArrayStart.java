package expressions.normal.array;

import expressions.special.Expression;
import parser.program.ExpressionType;

public class ArrayStart extends Expression {

	public ArrayStart(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.ARRAY_END, ExpressionType.ARRAY_START, ExpressionType.LITERAL, ExpressionType.NAME);
	}

}
