package expressions.normal;

import datatypes.Castable;
import expressions.special.Expression;
import expressions.special.ValueHolder;
import parser.program.ExpressionType;

/** Raw number, text or boolean value. */
public final class Literal extends Expression implements ValueHolder {

	private final Castable value;

	public Literal(Castable val, int line) {
		super(line);
		if(val == null)
			throw new NullPointerException("Value cannot be null!");
		this.value = val;
		setExpectedExpressions(ExpressionType.COMMA, ExpressionType.CLOSE_BRACKET, ExpressionType.ONE_LINE_STATEMENT,
				ExpressionType.OPEN_BLOCK, ExpressionType.INFIX_OPERATOR, ExpressionType.LOOP_CONNECTOR, ExpressionType.ARRAY_END);
	}

	@Override
	public Castable getValue() {
		return value;
	}

	@Override
	public String toString() {
		return "Literal " + value.toString();
	}

}
