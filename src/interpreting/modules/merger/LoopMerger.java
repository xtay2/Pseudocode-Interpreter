package interpreting.modules.merger;

import static building.types.specific.BuilderType.OPEN_BLOCK;
import static building.types.specific.BuilderType.STEP;
import static building.types.specific.BuilderType.TO;
import static building.types.specific.KeywordType.FROM;
import static building.types.specific.KeywordType.REPEAT;
import static building.types.specific.operators.InfixOpType.IN;
import static runtime.datatypes.numerical.ConceptualNrValue.POS_INF;
import static runtime.datatypes.numerical.NumberValue.ONE;
import static runtime.datatypes.numerical.NumberValue.ZERO;

import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.main.loops.ConditionalLoop;
import building.expressions.main.loops.ForEachLoop;
import building.expressions.main.loops.IntervalLoop;
import building.types.specific.KeywordType;
import interpreting.exceptions.IllegalCodeFormatException;

public abstract class LoopMerger extends SuperMerger {

	public static ConditionalLoop buildConditional(KeywordType type) {
		line.remove(0);
		return new ConditionalLoop(lineID, type, buildVal(), buildOpenBlock());
	}

	/** [FOR] [NAME] [IN] [CONTAINER] [OPEN_SCOPE] */
	public static ForEachLoop buildForEach() {
		line.remove(0);
		if (!line.remove(1).is(IN))
			throw new IllegalCodeFormatException(orgLine, "The For-Each-Loop has to contain the \"in\"-Keyword.");
		return new ForEachLoop(lineID, buildName(), buildVal(), buildOpenBlock());
	}

	/** [REPEAT] [REPETITIONS] [OPEN_SCOPE] */
	public static IntervalLoop buildRepeat() {
		line.remove(0);
		ValueHolder end = line.get(0).is(OPEN_BLOCK) ? POS_INF : buildVal();
		return new IntervalLoop(lineID, REPEAT, ZERO, end, ONE, buildOpenBlock());
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
			return new IntervalLoop(lineID, FROM, start, end, buildVal(), buildOpenBlock());
		} else
			return new IntervalLoop(lineID, FROM, start, end, ONE, buildOpenBlock());
	}
}
