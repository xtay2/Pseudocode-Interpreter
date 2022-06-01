package building.expressions.main.loops;

import static building.types.specific.KeywordType.REPEAT;
import static runtime.datatypes.numerical.ConceptualNrValue.POS_INF;
import static runtime.datatypes.numerical.NumberValue.ONE;

import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.normal.brackets.OpenBlock;
import building.expressions.normal.containers.Name;
import building.types.specific.KeywordType;
import errorhandeling.NonExpressionException;
import errorhandeling.PseudocodeException;
import runtime.datatypes.numerical.NumberValue;

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
		try {
			start = startHolder.asNr();
			end = endHolder.asNr();
			if (is(REPEAT) && end != POS_INF) {
				end = end.sub(ONE).asInt();
				if (end.isNegative())
					throw new PseudocodeException("ShouldBeNaturalNr", "Repeat-start cannot be negative.", getDataPath());
			}
			if (start.isSmallerEq(end))
				inc = incHolder.asNr().abs();
			else
				inc = incHolder.asNr().abs().negate();
		} catch (NonExpressionException e) {
			throw new PseudocodeException(e, getDataPath());
		}
	}

	@Override
	protected boolean doContinue(NumberValue iteration) {
		try {
			return start.isSmallerEq(end) ? iteration.isSmallerEq(end) : iteration.isGreaterEq(end);
		} catch (NonExpressionException e) {
			throw new PseudocodeException(e, getDataPath());
		}
	}
}
