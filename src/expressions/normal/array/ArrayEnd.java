package expressions.normal.array;

import expressions.special.Expression;
import helper.Output;
import parsing.program.ExpressionType;

public class ArrayEnd extends Expression {

	public ArrayEnd(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.COMMA, ExpressionType.CLOSE_BRACKET, ExpressionType.DECLARATION, ExpressionType.NAME,
				ExpressionType.INFIX_OPERATOR, ExpressionType.ARRAY_END, ExpressionType.ARRAY_START, ExpressionType.OPEN_BLOCK,
				ExpressionType.DEFINITE_LINEBREAK);
	}

	@Override
	public String toString() {
		return Output.DEBUG ? this.getClass().getSimpleName() : "']'";
	}
}
