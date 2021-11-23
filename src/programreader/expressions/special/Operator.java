package programreader.expressions.special;

import programreader.expressions.normal.operators.AddOperator;
import programreader.expressions.normal.operators.DivOperator;
import programreader.expressions.normal.operators.MultOperator;
import programreader.expressions.normal.operators.SubOperator;
import programreader.program.ExpressionType;

public abstract class Operator extends Expression implements ValueHolder {

	public Operator(int line) {
		super(line);
		setExpectedExpressions(ExpressionType.LITERAL, ExpressionType.NAME);
	}

	protected abstract Value perform(ValueHolder a, ValueHolder b);

	enum InfixOperator {
		ADD('+'), SUB('-'), MULT('*'), DIV('/');

		public final char symbol;

		InfixOperator(char c) {
			symbol = c;
		}
	}

	public static boolean isOperator(char c) {
		for (InfixOperator op : InfixOperator.values()) {
			if (op.symbol == c)
				return true;
		}
		return false;
	}

	public static Operator operatorExpression(char c, int line) {
		if(c == InfixOperator.ADD.symbol)
			return new AddOperator(line);
		if(c == InfixOperator.SUB.symbol)
			return new SubOperator(line);
		if(c == InfixOperator.MULT.symbol)
			return new MultOperator(line);
		if(c == InfixOperator.DIV.symbol)
			return new DivOperator(line);
		throw new AssertionError(c + " should be known by now.");
	}

}
