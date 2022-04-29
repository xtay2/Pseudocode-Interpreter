package building.expressions.main.loops;

import static building.types.specific.KeywordType.REPEAT;
import static runtime.datatypes.numerical.ConceptualNrValue.POS_INF;
import static runtime.datatypes.numerical.NumberValue.ONE;

import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.normal.brackets.OpenBlock;
import building.expressions.normal.containers.Name;
import building.types.specific.KeywordType;
import runtime.datatypes.numerical.NumberValue;
import runtime.exceptions.ShouldBeNaturalNrException;

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

	public IntervalLoop(int lineID, KeywordType loopType, ValueHolder startH, ValueHolder endH, ValueHolder incH, Name alias,
			OpenBlock os) {
		super(lineID, loopType, alias, os);
		assert type == KeywordType.FROM || type == KeywordType.REPEAT : "LoopType has to be either from or repeat.";
		startHolder = startH;
		endHolder = endH;
		incHolder = incH;
	}

	@Override
	protected void initLoop() {
		start = startHolder.getValue().asNumber();
		end = endHolder.getValue().asNumber();
		if (is(REPEAT) && end != POS_INF) {
			end = end.sub(ONE).asInt();
			if (end.isNegative())
				throw new ShouldBeNaturalNrException(getOriginalLine(), "Repeat-start cannot be negative.");
		}

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
