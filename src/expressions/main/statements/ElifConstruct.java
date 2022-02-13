package expressions.main.statements;

import expressions.abstractions.Expression;

/**
 * Groups the {@link IfStatement}, {@link ElifStatement} and {@link ElseStatement}.
 */
public interface ElifConstruct extends Statement {

	/** Returns the linenr after the last elif/else in this construct */
	int endOfConstruct();

	/** Initialises the following elif / else statement. */
	void setNextElse(ElifConstruct nextElse);

	@Override
	/** Merges from an optional BoolExpression and a OpenScope */
	void merge(Expression... e);
}
