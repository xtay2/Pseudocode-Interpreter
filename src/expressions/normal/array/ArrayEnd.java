package expressions.normal.array;

import static parsing.program.ExpressionType.*;

import expressions.normal.Expression;

public class ArrayEnd extends Expression {

	public ArrayEnd(int line) {
		super(line);
		setExpectedExpressions(COMMA, CLOSE_BRACKET, ASSIGNMENT, NAME, INFIX_OPERATOR, ARRAY_END, ARRAY_START, OPEN_SCOPE, CREMENT);
	}
}
