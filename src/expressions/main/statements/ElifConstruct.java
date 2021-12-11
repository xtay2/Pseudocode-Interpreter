package expressions.main.statements;

import expressions.special.Scope;

public interface ElifConstruct extends Scope {

	/** Returns the linenr after the last elif/else in this construct */
	int endOfConstruct();

	/** Initialises the following elif / else statement. */
	void setNextElse(ElifConstruct nextElse);

}
