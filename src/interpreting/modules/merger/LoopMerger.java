package interpreting.modules.merger;

import static building.types.specific.BuilderType.*;
import static building.types.specific.KeywordType.*;
import static building.types.specific.operators.InfixOpType.*;
import static runtime.datatypes.numerical.ConceptualNrValue.*;
import static runtime.datatypes.numerical.NumberValue.*;

import building.expressions.abstractions.interfaces.*;
import building.expressions.main.loops.*;
import building.expressions.normal.containers.name.*;
import building.types.specific.*;
import errorhandeling.*;

public abstract class LoopMerger extends SuperMerger {
	
	public static ConditionalLoop buildConditional(KeywordType type) {
		line.remove(0);
		return new ConditionalLoop(lineID, type, buildVal(), buildAlias(), buildOpenBlock());
	}
	
	/** [FOR] [NAME] [IN] [CONTAINER] [OPEN_SCOPE] */
	public static ForEachLoop buildForEach() {
		line.remove(0);
		if (!line.remove(1).is(IN))
			throw new PseudocodeException("IncompleteLoop", "The for-each-loop has to contain the \"in\"-Keyword.", path);
		return new ForEachLoop(lineID, buildName(VarName.class), buildVal(), buildOpenBlock());
	}
	
	/** [REPEAT] [REPETITIONS] [OPEN_SCOPE] */
	public static IntervalLoop buildRepeat() {
		line.remove(0);
		ValueHolder end = line.get(0).is(OPEN_BLOCK) ? POS_INF : buildVal();
		return new IntervalLoop(lineID, REPEAT, ZERO, end, ONE, buildAlias(), buildOpenBlock());
	}
	
	/** [FROM] [NUMBER] [TO] [NUMBER] (?[STEP] [INTERVALL]) */
	public static IntervalLoop buildFromTo() {
		line.remove(0);
		ValueHolder start = buildVal();
		if (!line.remove(0).is(TO)) // To-Keyword
			throw new PseudocodeException("IncompleteLoop", "Missing \"to\"-Keyword in from-to-loop.", path);
		ValueHolder end = buildVal();
		ValueHolder step = ONE;
		if (line.get(0).is(STEP)) {
			line.remove(0); // Step-Keyword
			step = buildVal();
		}
		return new IntervalLoop(lineID, FROM, start, end, step, buildAlias(), buildOpenBlock());
	}
	
	/** [AS] [NAME] */
	private static Name buildAlias() {
		if (line.get(0).is(AS)) {
			line.remove(0);
			return buildName(VarName.class);
		}
		return null;
	}
}
