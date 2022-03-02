package expressions.main.loops;

import datatypes.numerical.NumberValue;
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
	private final ValueHolder endHolder;

	/** Should get initialised at {@link #initLoop()}. */
	private NumberValue end;

	public IntervalLoop(int lineID, KeywordType loopType, ValueHolder startH, ValueHolder endH, ValueHolder incH, OpenScope os) {
		super(lineID, loopType, os);
		if (type != KeywordType.FROM && type != KeywordType.REPEAT)
			throw new AssertionError("LoopType has to be either from or repeat.");
		startHolder = startH;
		endHolder = endH;
		incHolder = incH;
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
