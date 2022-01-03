package expressions.normal.array;

import expressions.special.Expression;
import helper.Output;
import parser.program.ExpressionType;

public class ArrayStart extends Expression {

	public ArrayStart(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.ARRAY_END, ExpressionType.ARRAY_START, ExpressionType.LITERAL, ExpressionType.NAME);
	}

	@Override
	public String toString() {
		return Output.DEBUG ? this.getClass().getSimpleName() : "'['";
	}
}
