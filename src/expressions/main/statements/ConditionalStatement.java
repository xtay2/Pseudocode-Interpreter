package expressions.main.statements;

import static types.ExpressionType.LITERAL;
import static types.ExpressionType.NAME;
import static types.specific.BuilderType.ARRAY_START;
import static types.specific.KeywordType.ELIF;
import static types.specific.KeywordType.ELSE;
import static types.specific.KeywordType.IF;

import expressions.abstractions.Expression;
import expressions.abstractions.ScopeHolder;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.main.loops.ConditionalLoop;
import expressions.normal.brackets.OpenScope;
import interpreter.Interpreter;
import parsing.program.ProgramLine;
import types.specific.KeywordType;

/**
 * If-, Elif- and Else-Statement.
 * 
 * @see ScopeHolder
 * @see ConditionalLoop
 */
public final class ConditionalStatement extends ScopeHolder implements Statement {

	private ValueHolder condition;
	private ConditionalStatement nextElse;

	/**
	 * Creates a {@link ConditionalStatement}, based on the passed {@link KeywordType}.
	 * 
	 * @param lineID is the identifier of the matching {@link ProgramLine}.
	 * @param myType is the identifying Type, eiter {@link KeywordType#IF}, {@link KeywordType#ELIF} or
	 *               {@link KeywordType#ELSE}.
	 */
	public ConditionalStatement(int lineID, KeywordType myType) {
		super(lineID, myType, NAME, LITERAL, ARRAY_START);
		if (myType != IF && myType != ELIF && myType != ELSE)
			throw new AssertionError("Type has to be if, elif or else.");
	}

	@Override
	/** Merges from an optional Condition and a OpenScope. */
	public void merge(Expression... e) {
		if (e.length > 2)
			throw new AssertionError("This Statement only accepts one bool-Expression and one Scope.");
		// Condition and Scope
		if (!is(ELSE)) { // IF / ELIF
			condition = (ValueHolder) e[0];
			initScope((OpenScope) e[1]);
		} else // ELSE
			initScope((OpenScope) e[0]);
	}

	@Override
	public boolean execute(ValueHolder... params) {
		if (condition.getValue().asBool().value) {
			getScope().reg();
			if (!Interpreter.execute(lineIdentifier + 1)) {
				getScope().del();
				return false; // Wenn durch return abgebrochen wurde, rufe nichts hinter dem Block auf.
			}
			getScope().del();
		}
		return Interpreter.execute(endOfConstruct());
	}

	/** Initialises the following elif / else statement. */
	public void setNextElse(ConditionalStatement nextElse) {
		if (this.nextElse != null || nextElse.is(IF) || is(ELSE))
			throw new AssertionError("Trying an invalid connection with this Statement.");
		this.nextElse = nextElse;
	}

	/** Returns the linenr after the last elif/else in this construct. */
	public int endOfConstruct() {
		if (nextElse != null)
			return nextElse.endOfConstruct();
		return getScope().getEnd();
	}

}
