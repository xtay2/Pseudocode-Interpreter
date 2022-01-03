package expressions.normal;

import datatypes.Value;
import expressions.special.Expression;
import expressions.special.ValueHolder;
import helper.Output;
import parser.program.ExpressionType;

/** Raw number, text or boolean value. */
public final class Literal extends Expression implements ValueHolder {

	private final Value value;

	public Literal(Value val, int line) {
		super(line);
		if (val == null)
			throw new NullPointerException("Value cannot be null!");
		this.value = val;
		setExpectedExpressions(ExpressionType.COMMA, ExpressionType.CLOSE_BRACKET, ExpressionType.OPEN_BLOCK, ExpressionType.INFIX_OPERATOR,
				ExpressionType.LOOP_CONNECTOR, ExpressionType.ARRAY_END, ExpressionType.DEFINITE_LINEBREAK);
	}

	@Override
	public Value getValue() {
		return value;
	}

	@Override
	public String toString() {
		return Output.DEBUG ? this.getClass().getSimpleName() : value.toString();
	}

}
