package building.expressions.main.loops;

import static building.types.specific.KeywordType.*;

import building.expressions.abstractions.interfaces.*;
import building.expressions.abstractions.scopes.*;
import building.expressions.main.statements.*;
import building.expressions.normal.brackets.*;
import building.expressions.normal.containers.name.*;
import building.types.specific.*;
import errorhandeling.*;
import interpreting.program.*;
import runtime.datatypes.numerical.*;

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
		try {
			return condition.asBool().value == is(WHILE);
		} catch (NonExpressionException e) {
			throw new PseudocodeException(e, getBlueprintPath());
		}
	}
}
