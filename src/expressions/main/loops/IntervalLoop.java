package expressions.main.loops;

import static types.specific.BuilderType.ARRAY_START;
import static types.specific.ExpressionType.LITERAL;
import static types.specific.ExpressionType.NAME;

import datatypes.numerical.NumberValue;
import expressions.abstractions.Expression;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.brackets.OpenScope;
import types.specific.KeywordType;

/**
 * Formerly FromTo- / Repeat-Loop.
 * 
 * Iterates in steps from one number-value to another.
 */
public class IntervalLoop extends Loop {

	/** Should get initialised at merge. */
	private ValueHolder endHolder;

	/** Should get initialised at {@link #initLoop()}. */
	private NumberValue end;

	public IntervalLoop(int lineID, KeywordType loopType) {
		super(lineID, loopType, LITERAL, NAME, ARRAY_START);
		if (type != KeywordType.FROM && type != KeywordType.REPEAT)
			throw new AssertionError("LoopType has to be either from or repeat.");
	}

	/** [START_VALUE] [TO_VALUE] [STEP_VALUE]) [OPEN_SCOPE] */
	@Override
	public void merge(Expression... e) {
		if (e.length != 4)
			throw new AssertionError("The From-To-Loop expects a start, end, increment and scope.");
		startHolder = (ValueHolder) e[0];
		endHolder = (ValueHolder) e[1];
		incHolder = (ValueHolder) e[2];
		initScope((OpenScope) e[3]);
	}

	@Override
	protected void initLoop() {
		start = startHolder.getValue().asNumber();
		end = endHolder.getValue().asNumber();
		if (start.isSmallerEq(end))
			inc = incHolder.getValue().asNumber().abs();
		else
			inc = incHolder.getValue().asNumber().abs().negate();
	}

	@Override
	protected boolean doContinue(NumberValue iteration) {
		return start.isSmallerEq(end) ? iteration.isSmallerEq(end) : iteration.isGreaterEq(end);
	}
}
