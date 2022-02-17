package expressions.main.loops;

import static types.ExpressionType.LITERAL;
import static types.ExpressionType.NAME;
import static types.specific.BuilderType.ARRAY_START;
import static types.specific.KeywordType.UNTIL;
import static types.specific.KeywordType.WHILE;

import datatypes.numerical.NumberValue;
import expressions.abstractions.Expression;
import expressions.abstractions.ScopeHolder;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.main.statements.ConditionalStatement;
import expressions.normal.brackets.OpenScope;
import parsing.program.ProgramLine;
import types.specific.KeywordType;

/**
 * While/Until Loop.
 * 
 * @see ScopeHolder
 * @see ConditionalStatement
 */
public final class ConditionalLoop extends Loop {

	private ValueHolder condition;

	/**
	 * Creates a {@link ConditionalLoop}, based on the passed {@link KeywordType}.
	 * 
	 * @param lineID is the identifier of the matching {@link ProgramLine}.
	 * @param myType is the identifying Type, eiter {@link KeywordType#WHILE} or
	 *               {@link KeywordType#UNTIL}.
	 */
	public ConditionalLoop(int lineID, KeywordType myType) {
		super(lineID, myType, NAME, LITERAL, ARRAY_START);
		if (myType != WHILE && myType != UNTIL)
			throw new AssertionError("Type has to be while or until.");
	}

	/** [Condition] [OPEN_SCOPE] */
	@Override
	public void merge(Expression... e) {
		if (e.length != 2)
			throw new AssertionError("Merge on a while-/until-statement has to contain a condition and an opened scope.");
		condition = (ValueHolder) e[0];
		initScope((OpenScope) e[1]);
	}

	/**
	 * Returns true when:
	 * 
	 * <pre>
	 * while && cond == true
	 * or
	 * until && cond == false
	 * </pre>
	 */
	@Override
	protected boolean doContinue(NumberValue iteration) {
		return condition.getValue().asBool().value == is(WHILE);
	}
}
