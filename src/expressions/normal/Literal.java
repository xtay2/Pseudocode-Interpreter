package expressions.normal;

import expressions.special.Expression;
import expressions.special.Value;
import expressions.special.ValueHolder;
import parser.program.ExpressionType;

public final class Literal extends Expression implements ValueHolder {

	private final Value value;

	public Literal(String arg, int line) {
		super(line);
		value = new Value(arg);
		setExpectedExpressions(ExpressionType.COMMA, ExpressionType.CLOSE_BRACKET, ExpressionType.ONE_LINE_STATEMENT,
				ExpressionType.OPEN_BLOCK, ExpressionType.INFIX_OPERATOR, ExpressionType.LOOP_CONNECTOR);
	}

	@Override
	public Value getValue() {
		return value;
	}

	@Override
	public String toString() {
		return "Literal " + value.toString();
	}

}
