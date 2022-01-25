package expressions.normal.array;

import static parsing.program.ExpressionType.ARRAY_END;
import static parsing.program.ExpressionType.ARRAY_START;
import static parsing.program.ExpressionType.CREMENT;
import static parsing.program.ExpressionType.LITERAL;
import static parsing.program.ExpressionType.NAME;
import static parsing.program.ExpressionType.OPEN_BRACKET;

import expressions.normal.Expression;

public class ArrayStart extends Expression {

	public ArrayStart(int line) {
		super(line);
		setExpectedExpressions(ARRAY_END, ARRAY_START, LITERAL, NAME, OPEN_BRACKET, CREMENT);
	}
}
