package expressions.normal;

import static parsing.program.ExpressionType.ARRAY_START;
import static parsing.program.ExpressionType.EXPECTED_TYPE;
import static parsing.program.ExpressionType.LITERAL;
import static parsing.program.ExpressionType.NAME;

import expressions.special.Expression;
import helper.Output;

public class Comma extends Expression {

	public Comma(int line) {
		super(line);
		setExpectedExpressions(EXPECTED_TYPE, LITERAL, NAME, ARRAY_START);
	}

	@Override
	public String toString() {
		return Output.DEBUG ? this.getClass().getSimpleName() : "','";
	}
}
