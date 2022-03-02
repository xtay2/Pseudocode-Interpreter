package expressions.abstractions.interfaces;

import expressions.abstractions.Expression;
import expressions.abstractions.Scope;
import expressions.main.CloseScope;
import expressions.normal.brackets.OpenScope;

/**
 * An interface for any {@link Scope}-Bracket, that has to be connected with the match.
 * 
 * @see OpenScope
 * @see CloseScope
 */
public interface ScopeBracket {

	/** Returns the {@link Expression#lineIdentifier} of the matching {@link ScopeBracket}. */
	public int getMatch();
}
