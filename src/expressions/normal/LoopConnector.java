package expressions.normal;

import expressions.special.Expression;
import helper.Output;
import parser.program.ExpressionType;

public class LoopConnector extends Expression {

	public LoopConnector(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.LITERAL, ExpressionType.NAME, ExpressionType.ARRAY_START);
	}

	@Override
	public String toString() {
		return Output.DEBUG ? this.getClass().getSimpleName() : "range";
	}
}
