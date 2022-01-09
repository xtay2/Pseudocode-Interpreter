package expressions.main.statements;

public interface ElifConstruct {

	/** Returns the linenr after the last elif/else in this construct */
	int endOfConstruct();

	/** Initialises the following elif / else statement. */
	void setNextElse(ElifConstruct nextElse);

}
