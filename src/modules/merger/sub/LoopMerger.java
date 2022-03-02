package modules.merger.sub;

import static datatypes.numerical.ConceptualNrValue.POS_INF;
import static datatypes.numerical.NumberValue.ONE;
import static datatypes.numerical.NumberValue.ZERO;
import static types.specific.BuilderType.OPEN_SCOPE;
import static types.specific.BuilderType.STEP;
import static types.specific.BuilderType.TO;
import static types.specific.KeywordType.FROM;
import static types.specific.KeywordType.REPEAT;
import static types.specific.operators.InfixOpType.IN;

import exceptions.parsing.IllegalCodeFormatException;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.main.loops.ConditionalLoop;
import expressions.main.loops.ForEachLoop;
import expressions.main.loops.IntervalLoop;
import modules.merger.SuperMerger;
import types.specific.KeywordType;

public abstract class LoopMerger extends SuperMerger {

	public static ConditionalLoop buildConditional(KeywordType type) {
		line.remove(0);
		return new ConditionalLoop(lineID, type, buildVal(), buildOpenScope());
	}

	/** [FOR] [NAME] [IN] [CONTAINER] [OPEN_SCOPE] */
	public static ForEachLoop buildForEach() {
		line.remove(0);
		if (!line.remove(1).is(IN))
			throw new IllegalCodeFormatException(orgLine, "The For-Each-Loop has to contain the \"in\"-Keyword.");
		return new ForEachLoop(lineID, buildName(), buildVal(), buildOpenScope());
	}

	/** [REPEAT] [REPETITIONS] [OPEN_SCOPE] */
	public static IntervalLoop buildRepeat() {
		line.remove(0);
		ValueHolder end = line.get(0).is(OPEN_SCOPE) ? POS_INF : buildVal();
		return new IntervalLoop(lineID, REPEAT, ZERO, end, ONE, buildOpenScope());
	}

	/** [FROM] [NUMBER] [TO] [NUMBER] (?[STEP] [INTERVALL]) */
	public static IntervalLoop buildFromTo() {
		line.remove(0);
		ValueHolder start = buildVal();
		if (!line.remove(0).is(TO)) // To-Keyword
			throw new IllegalCodeFormatException(orgLine, "Missing \"to\"-Keyword in from-to-loop.");
		ValueHolder end = buildVal();
		if (line.get(0).is(STEP)) {
			line.remove(0); // LoopConnector
			return new IntervalLoop(lineID, FROM, start, end, buildVal(), buildOpenScope());
		} else
			return new IntervalLoop(lineID, FROM, start, end, ONE, buildOpenScope());
	}
}
