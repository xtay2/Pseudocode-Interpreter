package expressions.main.statements;

import static types.specific.BuilderType.ARRAY_START;
import static types.specific.ExpressionType.LITERAL;
import static types.specific.ExpressionType.NAME;
import static types.specific.ExpressionType.OPEN_SCOPE;
import static types.specific.KeywordType.ANY;
import static types.specific.KeywordType.ELIF;
import static types.specific.KeywordType.ELSE;
import static types.specific.KeywordType.IF;

import exceptions.parsing.IllegalCodeFormatException;
import expressions.abstractions.Expression;
import expressions.abstractions.ScopeHolder;
import expressions.abstractions.interfaces.ValueHolder;
import expressions.main.loops.ConditionalLoop;
import expressions.normal.brackets.OpenScope;
import modules.interpreter.Interpreter;
import modules.parser.program.ProgramLine;
import types.specific.KeywordType;

/**
 * If-, Elif- Any- or Else-Statement.
 * 
 * @see ScopeHolder
 * @see ConditionalLoop
 */
public final class ConditionalStatement extends ScopeHolder implements Statement {

	private ValueHolder condition;
	private ConditionalStatement nextBlock;

	/**
	 * Creates a {@link ConditionalStatement}, based on the passed {@link KeywordType}.
	 * 
	 * @param lineID is the identifier of the matching {@link ProgramLine}.
	 * @param myType is the identifying Type, eiter {@link KeywordType#IF}, {@link KeywordType#ELIF}
	 *               {@link KeywordType#ANY} or {@link KeywordType#ELSE}.
	 */
	public ConditionalStatement(int lineID, KeywordType myType) {
		super(lineID, myType, LITERAL, NAME, ARRAY_START, OPEN_SCOPE, IF);
		if (myType != IF && myType != ELIF && myType != ANY && myType != ELSE)
			throw new AssertionError("Type has to be if, elif, any or else.");
	}

	@Override
	/** Merges from an optional Condition and a OpenScope. */
	public void merge(Expression... e) {
		if (e.length > 2)
			throw new AssertionError("This Statement only accepts one bool-Expression and one Scope.");
		// Condition and Scope
		if (e.length == 2 && (is(IF) || is(ELIF) || is(ANY))) { // IF / ELIF / ANY-IF
			condition = (ValueHolder) e[0];
			initScope((OpenScope) e[1]);
		} else // ANY / ELSE
			initScope((OpenScope) e[0]);
	}

	/** Initialises the following elif / any / else statement. */
	public void setNextBlock(ConditionalStatement nextBlock) {
		if (this.nextBlock != null)
			throw new AssertionError("Trying an invalid connection with this Statement.");
		if (nextBlock.is(IF))
			throw new AssertionError("If can only be at the top of an if/elif/any/else Construct.");
		if (is(ELSE))
			throw new IllegalCodeFormatException(getOriginalLine(), "Else cannot have a following " + nextBlock + "-Statement.");
		if (!is(ELIF) && nextBlock.is(ANY))
			throw new IllegalCodeFormatException(getOriginalLine(), "An any-Block can only follow an elif-Statement.");
		this.nextBlock = nextBlock;
	}

	@Override
	public boolean execute(ValueHolder... params) {
		if (is(IF) || is(ELIF)) {
			if (condition.getValue().asBool().value) // Execute after condition is true.
				return executeBody() ? Interpreter.execute(findAnyCase()) : false; // Find any or end if successfull.
			return Interpreter.execute(findElseCase()); // Find next else if not successfull.
		} else if (is(ANY) && condition != null) {
			if (condition.getValue().asBool().value) {
				if (!executeBody()) // If any-if condition is true
					return false; // Return
			}
			return Interpreter.execute(endOfConstruct()); // End, if condition was false or statement is done.
		} else // Execute without condition. ANY / ELSE
			return executeBody() ? Interpreter.execute(endOfConstruct()) : false; // Jump to end after execution
	}

	/**
	 * Execute the body of the current construct.
	 * 
	 * @return true if the search for following blocks should procede and false if a
	 *         {@link ReturnStatement} was triggered inside of this block.
	 */
	private boolean executeBody() {
		if (!callFirstLine()) {
			getScope().clear();
			return false; // Return Statement got triggered.
		}
		getScope().clear();
		return true; // No Return. Call next block.
	}

	/**
	 * Returns the lineID of the next elif- or else-Statement, or the end of the construct, if none
	 * exist.
	 */
	private int findElseCase() {
		if (nextBlock != null) {
			if (nextBlock.is(ELIF) || nextBlock.is(ELSE))
				return nextBlock.getScope().getStart();
			if (nextBlock.is(ANY))
				return nextBlock.findElseCase();
			throw new IllegalCodeFormatException(getOriginalLine(), "Illegal Construct. " + nextBlock + " after " + type);
		}
		return endOfConstruct();
	}

	/**
	 * Returns the lineID of the connected any-Statement, or the end of the construct, if none exist.
	 */
	private int findAnyCase() {
		if (nextBlock != null) {
			if (nextBlock.is(ELIF))
				return nextBlock.findAnyCase();
			if (nextBlock.is(ANY))
				return nextBlock.getScope().getStart();
		}
		return endOfConstruct();
	}

	/** Returns the lineID after the last elif/else in this construct. */
	private int endOfConstruct() {
		if (nextBlock != null)
			return nextBlock.endOfConstruct();
		return getScope().getEnd();
	}

}
