package expressions.main.loops;

import static datatypes.numerical.ConceptualNrValue.POS_INF;
import static types.ExpressionType.LITERAL;
import static types.ExpressionType.NAME;
import static types.specific.BuilderType.ARRAY_START;

import datatypes.numerical.NumberValue;
import expressions.abstractions.Expression;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.normal.brackets.OpenScope;
import parsing.program.ProgramLine;
import types.specific.KeywordType;

public class RepeatLoop extends Loop {

	private ValueHolder end;

	/**
	 * Creates a {@link RepeatLoop}.
	 * 
	 * @param lineID is the identifier of the matching {@link ProgramLine}.
	 */
	public RepeatLoop(int lineID) {
		super(lineID, KeywordType.REPEAT, NAME, LITERAL, ARRAY_START);
	}

	/** Merges from an optional ValueHolder and a OpenScope. */
	@Override
	public void merge(Expression... e) {
		end = e.length == 2 ? (ValueHolder) e[0] : POS_INF;
		initScope((OpenScope) e[e.length - 1]);
	}

	@Override
	protected boolean doContinue(NumberValue iteration) {
		return iteration.isSmallerThan(end.getValue().asNumber());
	}
}
