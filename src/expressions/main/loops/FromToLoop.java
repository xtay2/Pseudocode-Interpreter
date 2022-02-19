package expressions.main.loops;

import static types.specific.BuilderType.ARRAY_START;
import static types.specific.ExpressionType.LITERAL;
import static types.specific.ExpressionType.NAME;

import datatypes.numerical.NumberValue;
import expressions.abstractions.Expression;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.brackets.OpenScope;
import types.specific.KeywordType;

public class FromToLoop extends Loop {

	private ValueHolder end;

	public FromToLoop(int line) {
		super(line, KeywordType.FROM, LITERAL, NAME, ARRAY_START);
	}

	/** [FROM] [TO] [?INTERVALL]) [OPEN_SCOPE] */
	@Override
	public void merge(Expression... e) {
		if (e.length != 4)
			throw new AssertionError("The From-To-Loop expects a start, end, increment and scope.");
		start = (ValueHolder) e[0];
		end = (ValueHolder) e[1];
		inc = (ValueHolder) e[2];
		initScope((OpenScope) e[3]);
	}

	@Override
	protected boolean doContinue(NumberValue iteration) {
		NumberValue s = start.getValue().asNumber();
		NumberValue e = end.getValue().asNumber();
		NumberValue i = inc.getValue().asNumber().abs();
		return s.isSmallerThan(e) ? iteration.add(i).isSmallerEq(e) : iteration.sub(i).isGreaterEq(e);
	}
}
