package expressions.abstractions.interfaces;

import expressions.main.CloseScope;
import expressions.normal.brackets.OpenScope;

/**
 * An interface for any {@link Scope}-Bracket, that has to be connected with the
 * match.
 * 
 * @see OpenScope
 * @see CloseScope
 */
public interface ScopeBracket {

	/** Find the matching Bracket */
	public ScopeBracket getMatch();
}
