package building.expressions.main.loops;

import static building.types.specific.KeywordType.UNTIL;
import static building.types.specific.KeywordType.WHILE;

import building.expressions.abstractions.ScopeHolder;
import building.expressions.abstractions.interfaces.ValueHolder;
import building.expressions.main.statements.ConditionalStatement;
import building.expressions.normal.brackets.OpenBlock;
import building.expressions.normal.containers.Name;
import building.types.specific.KeywordType;
import interpreting.program.ProgramLine;
import runtime.datatypes.numerical.NumberValue;

/**
 * While/Until Loop.
 *
 * @see ScopeHolder
 * @see ConditionalStatement
 */
public final class ConditionalLoop extends Loop {

	private final ValueHolder condition;

	/**
	 * Creates a {@link ConditionalLoop}, based on the passed {@link KeywordType}.
	 *
	 * @param lineID is the identifier of the matching {@link ProgramLine}.
	 * @param myType is the identifying Type, eiter {@link KeywordType#WHILE} or
	 * {@link KeywordType#UNTIL}.
	 */
	public ConditionalLoop(int lineID, KeywordType myType, ValueHolder condition, Name alias, OpenBlock os) {
		super(lineID, myType, alias, os);
		if (myType != WHILE && myType != UNTIL)
			throw new AssertionError("Type has to be while or until.");
		if (condition == null)
			throw new AssertionError("Condition cannot be null.");
		this.condition = condition;
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
		return condition.asBool().value == is(WHILE);
	}
}
